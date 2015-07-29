{-# LANGUAGE LambdaCase #-}
module HSync.Server.Authorization where

import Control.Lens hiding (children)
import HSync.Server.Import
import qualified Data.Set as S
import HSync.Server.Realm

--------------------------------------------------------------------------------

requireAll              :: S.Set AccessRight -> Maybe HashedPassword
                        -> RealmId -> Path -> Handler AuthResult
requireAll reqs mpw ri p = f <$> gatherRights mpw ri p
  where
    f rs
      | reqs `S.isSubsetOf` rs = Authorized
      | otherwise              = AuthenticationRequired


gatherRights           :: Maybe HashedPassword -> RealmId -> Path
                       -> Handler (S.Set AccessRight)
gatherRights mpw  ri p = queryAcid (QueryRealm ri) >>= \case
    Nothing -> do setMessage "No such realm"
                  notFound
    Just r  -> do
                 mu <- maybeAuthId
                 let mui = (^.userId) <$> mu
                 pure $ gatherAll mui mpw p (r^.realmTree)


gatherAll             :: Maybe UserId -> Maybe HashedPassword -> Path -> RealmTree
                      -> S.Set AccessRight
gatherAll mui mpw p r = S.unions . map (\io -> gather io p r) . catMaybes $
                        [ Just AccessAnonymous
                        , AccessPassword <$> mpw
                        , AccessUser     <$> mui
                        ]


gather                    :: AccessOption -> Path -> RealmTree -> S.Set AccessRight
gather oi (Path [])     r = lookupAccessRights oi  $ r^.nodeData.accessPolicy
gather oi (Path (p:ps)) r = (lookupAccessRights oi $ r^.nodeData.accessPolicy)
                            `S.union`
                            (maybe mempty (gather oi (Path ps)) $
                               lookupByName p (r^.children))
