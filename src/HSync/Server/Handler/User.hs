{-# LANGUAGE TupleSections #-}
module HSync.Server.Handler.User where

import Control.Lens
-- import Data.Maybe(mapMaybe)
import HSync.Server.Import
import qualified Data.IxSet as I
import qualified Data.Map as M

--------------------------------------------------------------------------------

getListUsersR :: Handler Html
getListUsersR = do
  (UserIndex idx n) <- queryAcid QueryUserIndex
  let users = I.toList idx
  defaultLayout $(widgetFile "listUsers")


getListUserRealmsR    :: UserName -> Handler Html
getListUserRealmsR ui = do
    mu <- queryAcid (LookupUserByName ui)
    case mu of
      Nothing -> do setMessage "No such user"
                    notFound
      Just u  -> do
                    (Realms m _) <- queryAcid QueryRealms
                    let allRealms = mapMaybe (\ap ->
                                      let ri = ap^.accessPointRealm in
                                      (ri,) <$> M.lookup ri m)
                                  . toList $ u^.realms
                    defaultLayout $(widgetFile "listRealms")



getUserProfileR :: UserName -> Handler Html
getUserProfileR = undefined
