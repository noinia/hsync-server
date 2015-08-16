module HSync.Server.Authorization where

import Control.Lens hiding (children, un)
import HSync.Server.Import.NoFoundation
import qualified Data.Set as S
import HSync.Server.Realm
import HSync.Common.User

--------------------------------------------------------------------------------


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


authorizedIfSelf    :: (YesodAuth master, AuthId master ~ User)
                    => UserName -> HandlerT master IO AuthResult
authorizedIfSelf un = do
      un' <- fmap (^.userName) <$> maybeAuthId
      if Just un == un' then return Authorized
                        else return AuthenticationRequired
