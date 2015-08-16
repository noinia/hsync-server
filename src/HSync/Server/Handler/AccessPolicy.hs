{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
module HSync.Server.Handler.AccessPolicy where

import Control.Lens
import HSync.Server.Import
import HSync.Server.Handler.AcidUtils(getLastModified)
import qualified Data.Set as S
import qualified Data.Map as M


--------------------------------------------------------------------------------


getAccessPolicyR      :: RealmId -> Path -> Handler Html
getAccessPolicyR ri p = queryAcid (Access ri p) >>= \case
    Nothing   -> do setMessage "Path Not found"
                    notFound
    Just node -> getAccessPolicyR' ri p node

getAccessPolicyR'           :: RealmId -> Path -> RealmTree -> Handler Html
getAccessPolicyR' ri p node = do
    (widget,enctype) <- generateFormPost accessOptionTypeForm
    defaultLayout $(widgetFile "accessPolicy")
  where
    ap      = node^.nodeData.accessPolicy
    allOpts = uncurry AccessItem <$> ap^.accessOptions.to M.assocs

    allRights = [Read,Write]



accessOptionTypeForm :: Form AccessOptionType
accessOptionTypeForm = renderDivs $ areq accessOptionType "Access Option" Nothing


--------------------------------------------------------------------------------

-- | Select the right form to add an aCcess Item
postAddAccessItemStartR      :: RealmId -> Path -> Handler Html
postAddAccessItemStartR ri p = handleForm accessOptionTypeForm (AccessPolicyR ri p) h
  where
    h t = let form = accessItemForm' t in do
            (widget,enctype) <- generateFormPost form
            defaultLayout $(widgetFile "addAccessPolicyItem")

accessItemForm'   :: AccessOptionType -> Form AccessItem
accessItemForm' t = renderDivs $ f Nothing
  where
    f = case t of
          ByAnonimous -> byAnonymousForm
          ByPassword  -> byPasswordForm
          ByUser      -> byUserForm


postAddAccessItemR        :: AccessOptionType -> RealmId -> Path -> Handler Html
postAddAccessItemR t ri p = handleForm (accessItemForm' t) (AccessPolicyR ri p)
                                       (saveAccessItem ri p)

saveAccessItem         :: RealmId -> Path -> AccessItem -> Handler Html
saveAccessItem ri p ai = do
      elm <- getLastModified webClientId
      case elm of
        Left err -> setMessage $ "Error: " <> toHtml err
        Right lm -> do b <- updateAcid $ SetAccessPolicy lm ai ri p
                       when (not b) $ setMessage "Error: Could not save Access policy"
      redirect $ AccessPolicyR ri p

--------------------------------------------------------------------------------

okIcon :: Html
okIcon = [shamlet| <span .glyphicon .glyphicon-ok aria-hidden="true">|]



--------------------------------------------------------------------------------
-- | Handlers for a single AccessItem


postModifyAccessItemR         :: AccessItem -> RealmId -> Path -> Handler Html
postModifyAccessItemR ai ri p =
  handleForm (accessItemForm ai) (ModifyAccessItemR ai ri p) (saveAccessItem ri p)


getModifyAccessItemR         :: AccessItem -> RealmId -> Path -> Handler Html
getModifyAccessItemR ai ri p = modifyAccessPolicyWidget ai ri p >>= defaultLayout


modifyAccessPolicyWidget         :: AccessItem -> RealmId -> Path -> Handler Widget
modifyAccessPolicyWidget ai ri p = do
    (form,enctype) <- generateFormPost (accessItemForm ai)
    return $ $(widgetFile "accessItem")

accessItemForm    :: AccessItem -> Form AccessItem
accessItemForm ai = renderDivs $ case ai^.accessOption of
                      AccessAnonymous  -> byAnonymousForm (Just ai)
                      AccessPassword _ -> byPasswordForm  (Just ai)
                      AccessUser _     -> byUserForm      (Just ai)


--------------------------------------------------------------------------------
-- * Custom Fields

accessOptionType :: Field Handler AccessOptionType
accessOptionType = radioFieldList [ ("Anonymous",         ByAnonimous)
                                  , ("Password",          ByPassword)
                                  , ("User" :: Text,      ByUser)
                                  ]


accessRightsForm    :: Maybe AccessItem -> AForm Handler [AccessRight]
accessRightsForm mi = (fromMaybe []) <$>
  aopt accessRightsField "Access Rights" ((^.accessRights.to toList.to Just) <$> mi)


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

--------------------------------------------------------------------------------


-- | Handle a form f, using handler h. If there is some error redirect to r
handleForm       :: Form a -> Route App -> (a -> Handler Html) -> Handler Html
handleForm f r h = runFormPost f >>= \((res,_), _) ->
    case res of
      FormSuccess t   -> h t
      FormFailure err -> g ("Error: " <> (mconcat $ map toHtml err))
      FormMissing     -> g  "Error: No such form"
  where
    g m = setMessage m >> redirect r
