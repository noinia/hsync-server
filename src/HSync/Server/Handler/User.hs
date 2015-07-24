module HSync.Server.Handler.User where

import HSync.Server.Import
import qualified Data.IxSet as I

getListUsersR :: Handler Html
getListUsersR = do
  (UserIndex idx n) <- queryAcid QueryUserIndex
  let users = I.toList idx
  defaultLayout $(widgetFile "listUsers")
