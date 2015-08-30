module HSync.Server.Handler.User where

import Control.Lens hiding (un)
-- import Data.Maybe(mapMaybe)
import HSync.Server.Import
import HSync.Server.Handler.AcidUtils(queryRealmName)

import qualified Data.IxSet as I
import qualified Data.Map as M
import qualified Data.Bimap as BM

--------------------------------------------------------------------------------

getListUsersR :: Handler Html
getListUsersR = do
  (UserIndex idx n _) <- queryAcid QueryUserIndex
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



getUserProfileR    :: UserName -> Handler Html
getUserProfileR un = queryAcid (LookupUserByName un) >>= \case
    Nothing   -> notFound
    Just user -> do
      (addClientWidget,addClientEncType) <- generateFormPost addClientForm
      allRealms <- mapM (\ap -> (ap,) <$> queryRealmName (ap^.accessPointRealm))
                .  toList $ user^.realms
      defaultLayout $(widgetFile "userProfile")
      where
        allClients = user^.clients.to BM.toAscList



postAddClientR    :: UserName -> Handler Html
postAddClientR un = do
    ((result,_),_) <- runFormPost addClientForm
    case result of
      FormSuccess cn -> do _ <- updateAcid $ AddClient un cn
                           setMessage $ "Client " <> toHtml cn <> " added."
      FormFailure errs -> let e = mconcat $ map toHtml errs
                          in setMessage $ "Error. Could not add client " <> e
      FormMissing    -> setMessage "Error. No such form"
    redirect $ UserProfileR un


addClientForm :: Form ClientName
addClientForm = renderDivs $
                   areq clientNameField "Client Name" Nothing

clientNameField :: Field Handler ClientName
clientNameField = checkMMap (return . right . ClientName) (^.unClientName) textField
  where
    right :: a -> Either Text a
    right = Right
