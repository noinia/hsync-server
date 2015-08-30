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
                         , NotificationsAsOf(..)

                         , toNotification

                         , SetAccessPolicy(..)

                           -- * Re-exports from Common.Realm:
                         , Realm(Realm), realmTree
                         , Realm.RealmNodeData(RealmNodeData), versions, accessPolicy
                         , RealmTree, Realm.current', Realm.realmRoot
                         , Realm.lastExistingVersion
                         , children, nodeData, name, measurement
                         , HasVersions(..)
                         , lookupByName
                         , Realm.RealmName, Realm.realmName

                         , AccessPoint(AccessPoint), accessPointRealm, accessPointPath

                         , unOrdByName
                         , OrdByName(..)
                         ) where

import Data.Semigroup
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

import HSync.Common.DateTime(DateTime)
import HSync.Common.Notification
import HSync.Common.AccessPolicy
import HSync.Common.FileVersion
import Control.Monad.State.Class
import Control.Monad.Reader.Class
import Data.Acid(Query, Update, makeAcidic, liftQuery)
import Data.SafeCopy(base, deriveSafeCopy)
import Control.Lens hiding (Indexable, children)

import qualified Data.Map  as M
import qualified Data.Set  as S
import qualified Data.List as L

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
                       Realms m ni <- get
                       let r  = Realm $ newRealmTree rn lm pol
                       let rs = Realms (M.insert ni r m) (succ ni)
                       put rs
                       return (ni,r,rs)

queryRealms :: Query Realms Realms
queryRealms = ask

-- | Get the realm with the given realmId
queryRealm   :: RealmId -> Query Realms (Maybe Realm)
queryRealm i = M.lookup i . _realmMap <$> ask

-- | Query a path in a realm. Returns the Tree rooted at that node.
access      :: RealmId -> Path -> Query Realms (Maybe RealmTree)
access ri p = (>>= Realm.access p) <$> queryRealm ri


data SP a b = SP { fst' :: !a,  snd' :: !b}

-- | Reports notifications in the (sub)realm as of the given date d (in
-- increasing order of occurrance). If for a given path q, the object at q, has
-- changed multiple times since d, only the most recent notification is returned.
notificationsAsOf        :: DateTime -> RealmId -> Path
                         -> Query Realms [Notification]
notificationsAsOf d ri p = maybe mempty gather <$> access ri p
  where
    (Path q) +++ n = Path $ q ++ [n^.name]

    gather n = L.sort . snd' $ gather' n (SP p mempty)

    gather' n (SP q acc)
      | n^.measurement.unLMT.to getMax < d = SP q acc  -- no recent changes in this tree
      | otherwise                          = let z = SP (q +++ n) (insertMe q n acc) in
          foldr gather' z (n^..children.to S.toList.traverse.unOrdByName)

    -- If this node n has recently changed, add it to the accumulator
    insertMe q n acc = if n^.nodeData.headVersionLens.lastModified.modificationTime < d
                       then acc -- I did not change recently
                       else toNotification n ri q:acc -- I've changed

toNotification        :: RealmTree -> RealmId -> Path -> Notification
toNotification n ri p = notification old new ri p
  where
    new = n^.nodeData.headVersionLens
    old = n^?nodeData.versions.ix 1


-- | Helper to modify a realm, returns true iff it applied the function
modifyRealm     :: RealmId -> (Realm -> Realm) -> Update Realms Bool
modifyRealm i f = liftQuery (queryRealm i) >>= \case
                    Nothing -> pure False
                    Just r  -> updateRealm i (f r) >> pure True


-- | Given a realmId i and a Realm r, overwrite/insert r with id i
updateRealm     :: RealmId -> Realm -> Update Realms ()
updateRealm i r = modify (&realmMap %~ (M.insert i r))
-- TODO: Return if we updated the thing, i.e. allow for checking





checkAndUpdate                    :: RealmId
                                  -> Path
                                  -> FileKind -- ^ the current filekind. If the
                                              -- file kind does not match the
                                              -- operation is not executed, and
                                              -- we return a Nothing
                                  -> (Realm -> Realm)
                                  -> Update Realms (Either ErrorMessage Notification)
checkAndUpdate ri p currentKind f = do
    old <- fmap (^.nodeData.headVersionLens) <$> liftQuery (access ri p)
    let fk = maybe NonExistent (^.fileKind) old
    if fk /= currentKind then return $ Left "Wrong filekind"
      else do modify (&realmMap %~ (M.adjust f ri))
              (toE . fmap (mkNotif old . getFV)) <$> liftQuery (access ri p)

  where
    getFV = (^.nodeData.headVersionLens)
    toE   = maybe (Left "error?") Right

    mkNotif old new = notification old new ri p






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
                      -> Update Realms (Either ErrorMessage Notification)
addFile ri p currentKind m b s = checkAndUpdate ri p currentKind
                                                   (Realm.addFile p m b s)

-- | Add A Directory to the given realm
-- pre: Realm exists
addDirectory                    :: RealmId -> Path -> FileKind -> LastModified
                                -> Update Realms (Either ErrorMessage Notification)
addDirectory ri p currentKind m = checkAndUpdate ri p currentKind
                                                 (Realm.addDirectory p m)

-- | Delete an item (either a file or directory) given by the path from the given realm
-- pre: Realm exists
delete                    :: RealmId -> Path -> FileKind -> LastModified
                          -> Update Realms (Either ErrorMessage Notification)
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
                      , 'notificationsAsOf

                      , 'createRealm
                      , 'updateRealm

                      , 'queryRealm
                      , 'access

                      , 'addFile
                      , 'addDirectory
                      , 'delete

                      , 'setAccessPolicy
                      ])
