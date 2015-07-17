{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE LambdaCase #-}
module HSync.Server.LocalAuth where

import Prelude
import Data.Acid(EventState)
import Data.Maybe
import Control.Lens
import HSync.Common.Types
import HSync.Common.AcidState
import HSync.Server.Settings(widgetFile)
import HSync.Common.User
import HSync.Server.User

import Yesod
import Yesod.Auth


import qualified Yesod.Auth.Message as Msg


--------------------------------------------------------------------------------
-- | The Acid actions that actually validate/insert stuff

type AcidMonad m e = (Functor m, MonadIO m, HasAcidState m (EventState e))

-- | Given a (user,password) in plaintext, validate them against the
--   database values
validateUser       :: AcidMonad m LookupUserByName
                   => UserName -> HashedPassword -> m Bool
validateUser ui pw = maybe False checkPassword <$> queryAcid (LookupUserByName ui)
  where
    checkPassword u = pw == password u


userExists   :: AcidMonad m LookupUserByName => UserName -> m Bool
userExists u = isJust <$> queryAcid (LookupUserByName u)


--------------------------------------------------------------------------------
--  The Auth plugin

class YesodAuth master => YesodLocalAuth master where
  onRegister :: AuthId master -> HandlerT master IO ()








loginR, registerR :: AuthRoute
loginR    = PluginR "local" ["login"]
registerR = PluginR "local" ["register"]


localAuth :: ( AcidMonad (HandlerT master IO) LookupUserByName
             , YesodLocalAuth master
             , AuthId master ~ User
             )
             => AuthPlugin master
localAuth = AuthPlugin "local" dispatch login
  where
    dispatch "POST" ["login"]    = postLoginR    >>= sendResponse
    dispatch "GET"  ["register"] = getRegisterR  >>= sendResponse
    dispatch "POST" ["register"] = postRegisterR >>= sendResponse
    dispatch _      _            = notFound


    -- url = PluginR "local" []

    login authToMaster =
        [whamlet|
$newline never
  <form role="form" method="post"
       action="@{authToMaster loginR}">

    <div class="form-group">
      <input type="text" name="userIdent" placeholder="Username" class="form-control">

    <div class="form-group">
      <input type="password" name="password" placeholder="Password" class="form-control">

    <button type="submit" class="btn btn-success">Sign in
                  |]



--------------------------------------------------------------------------------
-- | Login

postLoginR :: ( RenderMessage master FormMessage
              , YesodAuth master
              , AcidMonad (HandlerT master IO) LookupUserByName
              )
           => HandlerT Auth (HandlerT master IO) TypedContent
postLoginR = do
    (u,hpw) <- lift $ runInputPost $ (,)
                        <$>                   ireq userNameField "userName"
                        <*> (fmap hPassword $ ireq textField     "password")
    b <- lift $ validateUser u hpw
    if b then validUser   u
         else invalidUser
  where
    hPassword = hashPassword . Password

    validUser u = lift $  setCredsRedirect $ Creds "local" (u^.unUserName) []
    invalidUser = loginErrorMessageI LoginR Msg.InvalidUsernamePass


--------------------------------------------------------------------------------
-- | Register

getRegisterR :: (RenderMessage master FormMessage
                , YesodAuth master
                )
             => HandlerT Auth (HandlerT master IO) Html
getRegisterR = do
    -- Generate the form to be displayed
    (widget, enctype) <- lift $ generateFormPost registerForm
    tp <- getRouteToParent
    lift $ authLayout $ do
        setTitleI Msg.RegisterLong
        $(widgetFile "register")


postRegisterR :: ( RenderMessage master FormMessage
                 , YesodLocalAuth master
                 , AuthId master ~ User
                 , AcidMonad (HandlerT master IO) InsertUser
                 )
              => HandlerT Auth (HandlerT master IO) TypedContent
postRegisterR = do
    ((result, _), _) <- lift $ runFormPost registerForm
    tp <- getRouteToParent
    case result of
      FormSuccess u -> lift $ tryInsert u (tp registerR)
      _             -> invalidInput
  where
    invalidInput = loginErrorMessageI registerR Msg.InvalidKey

tryInsert :: ( RenderMessage master FormMessage
             , YesodLocalAuth master
             , AuthId master ~ User
             , AcidMonad (HandlerT master IO) InsertUser
             )
             => RegisterUser -> Route master -> HandlerT master IO TypedContent
tryInsert ru redir = updateAcid (InsertUser ru) >>= \case
      Left err    -> loginErrorMessage redir err
      Right (u,_) -> do
                       onRegister u
                       setCredsRedirect $ Creds "local" (u^.userName.unUserName) []


registerForm :: (RenderMessage (HandlerSite m) FormMessage, MonadHandler m)
             => Html -> MForm m (FormResult RegisterUser, WidgetT (HandlerSite m) IO ())
registerForm = renderDivs $ do mkUser
                        <$> areq userNameField "userName" Nothing
                        <*> areq textField     "realName" Nothing
                        <*> areq passwordField "password" Nothing
  where
    mkUser n rn p = RegisterUser n (RealName rn) (hashPassword $ Password p)

--------------------------------------------------------------------------------
-- | Helper stuff

userNameField :: (RenderMessage (HandlerSite m) FormMessage, Monad m)
            => Field m UserName
userNameField = checkMMap (return . validateUserName) _unUserName textField
