{-# LANGUAGE LambdaCase #-}
module HSync.Server.Realm( Realms(Realms), realmMap, nextRealmId

                          -- * Acididic Operations
                         , QueryRealms(..)
                         , CreateRealm(..)
                         , QueryRealm(..)
                         , UpdateRealm(..)
                         , Access(..)
                         , AddFile(..)
                         , AddDirectory(..)
                         , Delete(..)


                         , SetAccessPolicy(..)

                           -- * Re-exports from Common.Realm:
                         , Realm(Realm), realmTree
                         , Realm.RealmNodeData(RealmNodeData), versions, accessPolicy
                         , RealmTree, Realm.current', Realm.realmRoot
                         , Realm.lastExistingVersion
                         , children, nodeData, name, measurement
                         , HasVersions(..)
                         , lookupByName

                         , AccessPoint(AccessPoint), accessPointRealm, accessPointPath

                         , unOrdByName
                         , OrdByName(..)
                         ) where

import Data.Monoid
import Data.Default
import Prelude
import           HSync.Common.Types
import HSync.Common.Realm( Realm(..), RealmTree, versions, accessPolicy
                         , newRealmTree, realmTree
                         , children, nodeData, name, measurement, HasVersions(..)
                         , OrdByName(..), unOrdByName, lookupByName

                         , AccessPoint(AccessPoint), accessPointRealm, accessPointPath
                         )
import qualified HSync.Common.Realm as Realm


import HSync.Common.AccessPolicy
import HSync.Common.FileVersion
import Control.Monad.State.Class
import Control.Monad.Reader.Class
import Data.Acid(Query, Update, makeAcidic, liftQuery)
import Data.SafeCopy(base, deriveSafeCopy)
import Control.Lens hiding (Indexable, children)
import qualified Data.Map as M

--------------------------------------------------------------------------------

data Realms = Realms { _realmMap    :: M.Map RealmId Realm
                     , _nextRealmId :: RealmId
                     }
              deriving (Show,Read)
$(deriveSafeCopy 0 'base ''Realms)
makeLenses ''Realms

instance Default Realms where
  def = Realms mempty (RealmId 0)



createRealm           :: RealmName -> LastModified -> AccessPolicy
                      -> Update Realms (RealmId,Realm,Realms)
createRealm rn lm pol = do
                       Realms m ni@(RealmId i) <- get
                       let r  = Realm $ newRealmTree rn lm pol
                       let rs = Realms (M.insert ni r m) (RealmId $ i+1)
                       put rs
                       return (ni,r,rs)

queryRealms :: Query Realms Realms
queryRealms = ask


queryRealm   :: RealmId -> Query Realms (Maybe Realm)
queryRealm i = M.lookup i . _realmMap <$> ask


modifyRealm     :: RealmId -> (Realm -> Realm) -> Update Realms Bool
modifyRealm i f = liftQuery (queryRealm i) >>= \case
                    Nothing -> pure False
                    Just r  -> updateRealm i (f r) >> pure True

updateRealm     :: RealmId -> Realm -> Update Realms ()
updateRealm i r = modify (&realmMap %~ (M.insert i r))


-- TODO: Return if we updated the thing, i.e. allow for checking

access      :: RealmId -> Path -> Query Realms (Maybe RealmTree)
access ri p = (>>= Realm.access p) <$> queryRealm ri


checkAndUpdate                    :: RealmId
                                  -> Path
                                  -> FileKind -- ^ the current filekind. If the
                                              -- file kind does not match the
                                              -- operation is not executed, and
                                              -- we return a Nothing
                                  -> (Realm -> Realm)
                                  -> Update Realms (Either ErrorMessage FileVersion)
checkAndUpdate ri p currentKind f = do
    fk <- maybe NonExistent (^.nodeData.headVersionLens.fileKind)
          <$> liftQuery (access ri p)
    if fk /= currentKind then return $ Left "Wrong filekind"
      else do modify (&realmMap %~ (M.adjust f ri))
              g <$> liftQuery (access ri p)
  where
    g   = toE . fmap (^.nodeData.headVersionLens)
    toE = maybe (Left "error?") Right




-- | Add A file(version) to the given realm. Note that this function is also
-- applicable in case of updates:
-- pre: Realm exists
addFile               :: RealmId
                      -> Path
                      -> FileKind -- ^ the current filekind. If the file kind does not match
                                  -- the operation is not executed, and we return a Nothing
                      -> LastModified -- ^ the new modification information
                      -> Bool -- ^ if we committed the data already or not
                      -> Signature -- ^ and the file's signature
                      -> Update Realms (Either ErrorMessage FileVersion)
addFile ri p currentKind m b s = checkAndUpdate ri p currentKind
                                                   (Realm.addFile p m b s)

-- | Add A Directory to the given realm
-- pre: Realm exists
addDirectory        :: RealmId -> Path
                    -> FileKind
                    -> LastModified -> Update Realms (Either ErrorMessage FileVersion)
addDirectory ri p currentKind m = checkAndUpdate ri p currentKind
                                                 (Realm.addDirectory p m)

-- | Delete an item (either a file or directory) given by the path from the given realm
-- pre: Realm exists
delete        :: RealmId -> Path
                 -> FileKind
                 -> LastModified -> Update Realms (Either ErrorMessage FileVersion)
delete ri p currentKind m = checkAndUpdate ri p currentKind
                                           (Realm.delete p m)


--------------------------------------------------------------------------------
-- * Access Policy stuff

setAccessPolicy                           :: LastModified -> AccessItem -> RealmId -> Path
                                          -> Update Realms Bool
setAccessPolicy lm (AccessItem o rs) ri p =
    modifyRealm ri (Realm.updateAccessPolicyOf p f lm)
  where
    f pol = pol&accessOptions %~ M.insert o rs

--------------------------------------------------------------------------------

$(makeAcidic ''Realms [ 'queryRealms

                      , 'createRealm
                      , 'updateRealm

                      , 'queryRealm
                      , 'access

                      , 'addFile
                      , 'addDirectory
                      , 'delete

                      , 'setAccessPolicy
                      ])
