{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
module HSync.Server.Handler.AccessPolicy where

import Control.Lens
import HSync.Server.Import
import qualified Data.Set as S


--------------------------------------------------------------------------------


getAccessPolicyR      :: RealmId -> Path -> Handler Html
getAccessPolicyR ri p = queryAcid (Access ri p) >>= \case
    Nothing   -> do setMessage "Path Not found"
                    notFound
    Just node -> getAccessPolicyR' ri p node

getAccessPolicyR'           :: RealmId -> Path -> RealmTree -> Handler Html
getAccessPolicyR' ri p node = defaultLayout $(widgetFile "accessPolicy")
  where
    ap = node^.nodeData.accessPolicy


getModifyAccessItemR         :: AccessItem -> RealmId -> Path -> Handler Html
getModifyAccessItemR ai ri p = modifyAccessPolicyWidget ai ri p >>= defaultLayout


modifyAccessPolicyWidget         :: AccessItem -> RealmId -> Path -> Handler Widget
modifyAccessPolicyWidget ai ri p = do
    (form,enctype) <- generateFormPost (accessItemForm ai)
    return $ $(widgetFile "accessItem")


postModifyAccessItemR :: AccessItem -> RealmId -> Path -> Handler Html
postModifyAccessItemR = undefined

accessItemForm ai = renderDivs $ case ai^.accessOption of
                      AccessAnonymous  -> byAnonymousForm (Just ai)
                      AccessPassword _ -> byPasswordForm  (Just ai)
                      AccessUser _     -> byUserForm      (Just ai)


accessOptionType :: Field Handler AccessOptionType
accessOptionType = radioFieldList [ ("Anonymous",         ByAnonimous)
                                  , ("Password",          ByPassword)
                                  , ("User" :: Text,      ByUser)
                                  ]



accessRightsForm mi =
  areq accessRightsField "Access Rights" ((^.accessRights.to toList) <$> mi)

sel       :: Maybe AccessItem -> Prism' AccessOption a -> Maybe a
sel mi pr = mi^?_Just.accessOption.pr

byAnonymousForm    :: Maybe AccessItem -> AForm Handler AccessItem
byAnonymousForm mi = (\rs -> AccessItem AccessAnonymous (S.fromList rs))
                  <$> accessRightsForm mi

byPasswordForm    :: Maybe AccessItem -> AForm Handler AccessItem
byPasswordForm mi = f <$> areq textField "Password" (sel mi (_AccessPassword.unHashedPassword))
                      <*> accessRightsForm mi
  where
    f pw rs = AccessItem (AccessPassword . hashPassword . Password $ pw) (S.fromList rs)

byUserForm    :: Maybe AccessItem -> AForm Handler AccessItem
byUserForm mi = f <$> areq userByUserNameField "Username" Nothing
                  <*> accessRightsForm mi
  where
    f u rs = AccessItem (AccessUser $ u^.userId) (S.fromList rs)


userByUserNameField :: Field Handler User
userByUserNameField = checkMMap f (^.userName.unUserName) textField
  where
    f   = either (pure . Left) g . validateUserName
    g n = maybe (Left "User not found") Right <$> queryAcid (LookupUserByName n)


accessRightsField :: Field Handler [AccessRight]
accessRightsField = checkboxesFieldList [ ("Read" :: Text,   Read)
                                        , ("Write",          Write)
                                        ]



-- accessItemForm    :: Maybe AccessItem -> Form AccessItem
-- accessItemForm ai = renderDivs $ (,) <$>



postAccessPolicyR :: RealmId -> Path -> Handler Html
postAccessPolicyR ri p = undefined
