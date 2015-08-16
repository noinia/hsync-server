{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module HSync.Common.Realm(
                           Realm(Realm), realmTree
                         , RealmNodeData(RealmNodeData), versions, accessPolicy
                         , RealmTree
                         , lastExistingVersion

                         , AccessPoint(AccessPoint), accessPointRealm, accessPointPath

                         , newRealmTree
                         , realmData
                         , realmRoot


                         , realmName, RealmName
                         , access
                         , accessWithRealmName

                         , current
                         , current'

                         , addFile
                         , addDirectory
                         , delete
                         , update

                         , commit
                         , commit'

                         , updateAccessPolicy
                         , updateAccessPolicyOf

                         , files
                         , subDirectories

                           -- * Re-Exports from StorageTree
                         , ST.HasVersions(..)

                         , ST.Measured(..)
                         , ST.HasEmpty(..)
                         , ST.OrdByName(..), ST.unOrdByName
                         , ST.lookupByName

                         , ST.StorageTree(Node), ST.name, ST.measurement
                         , ST.nodeData, ST.children
                         )

       where

import Data.Maybe(listToMaybe)
import qualified Data.List.NonEmpty as NE
import ClassyPrelude.Yesod hiding (update, delete)
import Control.Lens
import HSync.Common.Types
import HSync.Common.AccessPolicy
import HSync.Common.FileVersion
import HSync.Common.OrphanInstances()
import HSync.Common.StorageTree(StorageTree,OrdByName)
import qualified HSync.Common.StorageTree as ST
import qualified Data.Set as S
import Data.SafeCopy(base, deriveSafeCopy)

--------------------------------------------------------------------------------

data RealmNodeData v = RealmNodeData { _versions     :: NE.NonEmpty v
                                     , _accessPolicy :: AccessPolicy
                                     }
                     deriving (Show,Read,Eq)
makeLenses ''RealmNodeData
$(deriveSafeCopy 0 'base ''RealmNodeData)


realmData        :: v -> AccessPolicy -> RealmNodeData v
realmData fv pol = RealmNodeData (fv NE.:| []) pol

instance ST.HasVersions RealmNodeData v where
  headVersionLens = versions.lens NE.head (\(_ NE.:| xs) x -> x NE.:| xs)

  addVersion v  = versions %~ (v NE.<|)


instance ST.Measured m v => ST.Measured m (RealmNodeData v) where
  measure = ST.measure . NE.head . _versions


-- | This should really produce a Just; otherwise there would only be nonExistent versions
lastExistingVersion :: RealmNodeData FileVersion -> Maybe FileVersion
lastExistingVersion = listToMaybe . NE.dropWhile (not . exists . (^.fileKind))
                    . view versions




--------------------------------------------------------------------------------

type GRealmTree m v = StorageTree FileName m (RealmNodeData v)


-- | The tree of data we store
type RealmTree = GRealmTree LastModificationTime FileVersion


newRealmTree         :: RealmName -> LastModified -> AccessPolicy -> RealmTree
newRealmTree n m pol = ST.Node n (ST.measure d) d mempty
  where
    d = realmData (FileVersion Directory m True) pol


--------------------------------------------------------------------------------


newtype Realm = Realm { _realmTree :: RealmTree }
             deriving (Show,Read)
makeLenses ''Realm
$(deriveSafeCopy 0 'base ''Realm)

-- | Get the name of the realm, basically the name of the root of the node in
-- the realm tree.
realmName :: Lens' Realm RealmName
realmName = realmTree.ST.name

-- | prepends the realm name to the path
accessWithRealmName            :: Path -> Realm -> Maybe RealmTree
accessWithRealmName (Path p) r = access (Path $ r^.realmName : p) r

access          :: Path -> Realm -> Maybe RealmTree
access (Path p) = ST.access p . _realmTree

current :: Realm -> StorageTree FileName LastModificationTime FileVersion
current = current' . _realmTree

current' :: RealmTree -> StorageTree FileName LastModificationTime FileVersion
current' = fmap ST.headVersion


addFile         :: Path -> LastModified -> Bool -> Signature
                -> Realm -> Realm
addFile p m b s = realmTree %~ update p (FileVersion (File s) m b)

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
    parentData = realmData (FileVersion Directory lm True) mempty


-- | Update the access policy of the realm
updateAccessPolicy     :: (AccessPolicy -> AccessPolicy) -> Realm -> Realm
updateAccessPolicy f r = r&realmTree.ST.nodeData.accessPolicy %~ f

updateAccessPolicyOf          :: Path -> (AccessPolicy -> AccessPolicy)
                              -> LastModified -> Realm -> Realm
updateAccessPolicyOf p f lm r = r&realmTree %~ updateAccessPolicyAt p f lm


update            :: Path -> FileVersion -> RealmTree -> RealmTree
update (Path p) v = ST.updateVersionAt p (const v) parentData
  where
    parentData = realmData (FileVersion Directory (v^.lastModified) True) mempty


updateAccessPolicyAt               :: Path -> (AccessPolicy -> AccessPolicy)
                                   -> LastModified -- ^ Modification settings to
                                                   -- use for missing parents
                                   -> RealmTree -> RealmTree
updateAccessPolicyAt (Path p) f lm = ST.updateAt p (&accessPolicy %~ f) parentData
  where
    parentData = realmData (FileVersion Directory lm True) mempty

--------------------------------------------------------------------------------


data AccessPoint = AccessPoint { _accessPointRealm :: RealmId
                               , _accessPointPath  :: Path
                               }
                 deriving (Show,Read,Eq,Ord,Typeable)
makeLenses ''AccessPoint
$(deriveSafeCopy 0 'base ''AccessPoint)

realmRoot   :: RealmId -> AccessPoint
realmRoot i = AccessPoint i (Path [])


--------------------------------------------------------------------------------



files :: StorageTree n m FileVersion
      -> [StorageTree n m FileVersion]
files = childrenByKind isFile

subDirectories :: StorageTree n m FileVersion
               -> [StorageTree n m FileVersion]
subDirectories = childrenByKind isDirectory

childrenByKind p = fmap (^.ST.unOrdByName) . S.toAscList . childrenByKind' p

childrenByKind'     :: (FileKind -> Bool) -> StorageTree n m FileVersion
                   -> S.Set (OrdByName n (StorageTree n m FileVersion))
childrenByKind' p t = S.filter (^.ST.unOrdByName.ST.nodeData.fileKind.to p) $ t^.ST.children

--------------------------------------------------------------------------------
