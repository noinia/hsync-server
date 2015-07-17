{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module HSync.Common.Realm where

import qualified Data.List.NonEmpty as NE
import ClassyPrelude.Yesod hiding (update)
import Control.Lens
import HSync.Common.Types
import HSync.Common.AccessPolicy
import HSync.Common.FileVersion
import HSync.Common.OrphanInstances
import qualified HSync.Common.StorageTree as ST
import Data.SafeCopy(base, deriveSafeCopy)

--------------------------------------------------------------------------------

data RealmNodeData v = RealmNodeData { _versions     :: NE.NonEmpty v
                                     , _accessPolicy :: AccessPolicy
                                     }
                     deriving (Show,Read,Eq)
makeLenses ''RealmNodeData
$(deriveSafeCopy 0 'base ''RealmNodeData)


realmData    :: v -> RealmNodeData v
realmData fv = RealmNodeData (fv NE.:| []) []

instance ST.HasVersions RealmNodeData v where
  headVersionLens = versions.lens NE.head (\(_ NE.:| xs) x -> x NE.:| xs)

  addVersion v  = versions %~ (v NE.<|)


instance ST.Measured m v => ST.Measured m (RealmNodeData v) where
  measure = ST.measure . NE.head . _versions


type GRealmTree m v = ST.StorageTree FileName m (RealmNodeData v)


-- | The tree of data we store
type RealmTree = GRealmTree LastModificationTime FileVersion


newRealmTree     :: RealmName -> LastModified -> RealmTree
newRealmTree n m = ST.Node n (ST.measure d) d mempty
  where
    d = realmData $ FileVersion Directory m True


--------------------------------------------------------------------------------


data Realm = Realm { _realmTree         :: RealmTree
                   , _realmAccessPolicy :: AccessPolicy
                   }
             deriving (Show,Read)
makeLenses ''Realm
$(deriveSafeCopy 0 'base ''Realm)


addFile            :: Path -> LastModified -> Bool -> FilePath -> Signature
                   -> Realm -> Realm
addFile p m b fp s = realmTree %~ update p (FileVersion (File fp s) m b)

addDirectory     :: Path -> LastModified -> Realm -> Realm
addDirectory p m = realmTree %~ update p (FileVersion Directory m True)

delete       :: Path -> LastModified -> Realm -> Realm
delete p m r = r&realmTree %~ update p (FileVersion NonExistent m True)


-- | Set the commit bit, without creating a new version
commit       :: Path -> LastModified -> Realm -> Realm
commit p m r = r&realmTree %~ commit' p m


commit'             :: Path -> LastModified -> RealmTree -> RealmTree
commit' (Path p) lm = ST.updateAt p (&ST.headVersionLens.dataCommitted .~ True) parentData
  where
    parentData = realmData $ FileVersion Directory lm True


-- | Update the access policy of the realm
updateAccessPolicy     :: (AccessPolicy -> AccessPolicy) -> Realm -> Realm
updateAccessPolicy f r = r&realmAccessPolicy %~ f


updateAccessPolicyOf          :: Path -> (AccessPolicy -> AccessPolicy)
                              -> LastModified -> Realm -> Realm
updateAccessPolicyOf p f lm r = r&realmTree %~ updateAccessPolicyAt p f lm


update            :: Path -> FileVersion -> RealmTree -> RealmTree
update (Path p) v = ST.updateVersionAt p (const v) parentData
  where
    parentData = realmData $ FileVersion Directory (v^.lastModified) True


updateAccessPolicyAt               :: Path -> (AccessPolicy -> AccessPolicy)
                                   -> LastModified -- ^ Modification settings to
                                                   -- use for missing parents
                                   -> RealmTree -> RealmTree
updateAccessPolicyAt (Path p) f lm = ST.updateAt p (&accessPolicy %~ f) parentData
  where
    parentData = realmData $ FileVersion Directory lm True

--------------------------------------------------------------------------------


data AccessPoint = AccessPoint { _accessPointRealm :: RealmId
                               , _accessPointPath  :: Path
                               }
                 deriving (Show,Read,Eq,Ord,Typeable)
makeLenses ''AccessPoint
$(deriveSafeCopy 0 'base ''AccessPoint)

realmRoot   :: RealmId -> AccessPoint
realmRoot i = AccessPoint i (Path [])
