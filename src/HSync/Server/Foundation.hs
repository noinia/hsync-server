{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
module HSync.Server.Foundation where

import HSync.Server.Import.NoFoundation
import HSync.Server.Types
import Text.Hamlet          (hamletFile)
import Text.Jasmine         (minifym)
import Yesod.Auth.Message   (AuthMessage (InvalidLogin))
import Yesod.Default.Util   (addStaticContentExternal)
import Yesod.Core.Types     (Logger)
import qualified Yesod.Core.Unsafe as Unsafe
import Control.Lens
import HSync.Server.User
import HSync.Server.Realm
import HSync.Server.LocalAuth
import HSync.Server.AcidState
import HSync.Server.Authorization
import HSync.Common.AcidState
import HSync.Common.API

import qualified Data.Map as M
import qualified Data.Set as S

--------------------------------------------------------------------------------

-- | The foundation datatype for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
    { _appSettings    :: AppSettings
    , _appStatic      :: Static -- ^ Settings for static file serving.
    , _appAcids       :: Acids
    , _appHttpManager :: Manager
    , _appLogger      :: Logger
    }
makeLenses ''App


getStatic :: App -> Static
getStatic = _appStatic

getAcids :: HandlerT App IO Acids
getAcids = _appAcids <$> getYesod

getHSyncAPI :: App -> HSyncAPI
getHSyncAPI = const HSyncAPI


instance HasHttpManager App where
    getHttpManager = _appHttpManager

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/routing-and-handlers
--
-- Note that this is really half the story; in Application.hs, mkYesodDispatch
-- generates the rest of the code. Please see the linked documentation for an
-- explanation for this split.
--
-- This function also generates the following type synonyms:
-- type Handler = HandlerT App IO
-- type Widget = WidgetT App IO ()
mkYesodData "App" $(parseRoutesFile "config/routes")

-- | A convenient synonym for creating forms.
type Form x = Html -> MForm (HandlerT App IO) (FormResult x, Widget)

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
    -- Controls the base of generated URLs. For more information on modifying,
    -- see: https://github.com/yesodweb/yesod/wiki/Overriding-approot
    approot = ApprootMaster $ _appRoot . _appSettings

    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    makeSessionBackend _ = Just <$> defaultClientSessionBackend
        120    -- timeout in minutes
        "config/client_session_key.aes"

    defaultLayout mainContent = defaultLayout' $(widgetFile "simple-layout")

    -- The page to be redirected to when authentication is required.
    authRoute _ = Just $ AuthR LoginR

    -- Routes not requiring authentication.
    isAuthorized (AuthR _) _ = return Authorized
    isAuthorized FaviconR  _ = return Authorized
    isAuthorized RobotsR   _ = return Authorized
    isAuthorized HomeR     _ = return Authorized
    isAuthorized AboutR    _ = return Authorized

    isAuthorized (UserProfileR un) _    = authorizedIfSelf un
    isAuthorized (ListUserRealmsR un) _ = authorizedIfSelf un

    isAuthorized (APIR APILoginR)               _ = return Authorized

    isAuthorized (APIR (ListenNowR       ri   p)) _ = requireAll [Read] Nothing ri p
    isAuthorized (APIR (ListenR        _ ri   p)) _ = requireAll [Read] Nothing ri p
    isAuthorized (APIR (CurrentRealmR    ri   p)) _ = requireAll [Read] Nothing ri p
    isAuthorized (APIR (DownloadR        ri _ p)) _ = requireAll [Read] Nothing ri p
    isAuthorized (APIR (FileR            ri _ p)) _ = requireAll [Read] Nothing ri p
    isAuthorized (APIR (DownloadCurrentR ri   p)) _ = requireAll [Read] Nothing ri p

    isAuthorized (APIR (CreateDirR _ ri   p)) _ = requireAll [Read, Write] Nothing ri p
    isAuthorized (APIR (StoreFileR _ ri _ p)) _ = requireAll [Read, Write] Nothing ri p
    isAuthorized (APIR (DeleteR    _ ri _ p)) _ = requireAll [Read, Write] Nothing ri p


    isAuthorized (WebCreateDirR   ri   p) _ = requireAll [Read, Write] Nothing ri p
    isAuthorized (WebStoreFileR   ri   p) _ = requireAll [Read, Write] Nothing ri p
    isAuthorized (WebDeleteR      ri _ p) _ = requireAll [Read, Write] Nothing ri p

    isAuthorized (AccessPolicyR       ri p) _ = requireAll [Read, Write] Nothing ri p
    isAuthorized (ModifyAccessItemR _ ri p) _ = requireAll [Read, Write] Nothing ri p

    isAuthorized (AddAccessItemStartR  ri p) _ = requireAll [Read, Write] Nothing ri p
    isAuthorized (AddAccessItemR     _ ri p) _ = requireAll [Read, Write] Nothing ri p



    isAuthorized (ViewRealmR ri p) _ = requireAll [Read] Nothing ri p

    -- TODO: this is not a great idea
    isAuthorized ListUsersR           _ = return Authorized

    isAuthorized _ _ = return AuthenticationRequired

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent ext mime content = do
        master <- getYesod
        let staticDir = _staticDir $ _appSettings master
        addStaticContentExternal
            minifym
            genFileName
            staticDir
            (StaticR . flip StaticRoute [])
            ext
            mime
            content
      where
        -- Generate a unique filename based on the content itself
        genFileName lbs = "autogen-" ++ base64md5 lbs

    -- What messages should be logged. The following includes all messages when
    -- in development, and warnings and errors in production.
    shouldLog app _source level =
        app^.appSettings.shouldLogAll
            || level == LevelWarn
            || level == LevelError

    makeLogger = return . _appLogger

    maximumContentLength _ (Just r)
                     | postsBigFile r = Nothing -- no limits for big  files
      where
        postsBigFile (APIR (StoreFileR _ _ _ _))  = True
        postsBigFile (WebStoreFileR _ _)          = True
        postsBigFile _                            = False
    maximumContentLength _ _          = Just $ 2 * 1024 * 1024 -- 2 megabytes


hasRights               :: [AccessRight] -> Maybe HashedPassword -> RealmId -> Path
                        -> Handler Bool
hasRights reqs mpw ri p = f <$> gatherRights mpw ri p
  where
    f rs = S.fromList reqs `S.isSubsetOf` rs

requireAll              :: [AccessRight] -> Maybe HashedPassword
                        -> RealmId -> Path -> Handler AuthResult
requireAll reqs mpw ri p = f <$> gatherRights mpw ri p
  where
    f rs
      | (S.fromList reqs) `S.isSubsetOf` rs = Authorized
      | otherwise                           = AuthenticationRequired


gatherRights           :: Maybe HashedPassword -> RealmId -> Path
                       -> Handler (S.Set AccessRight)
gatherRights mpw  ri p = queryAcid (QueryRealm ri) >>= \case
    Nothing -> do setMessage "No such realm"
                  notFound
    Just r  -> do
                 mu <- maybeAuthId
                 let mui = (^.userId) <$> mu
                 pure $ gatherAll mui mpw p (r^.realmTree)


defaultLayout'        :: Widget -> Handler Html
defaultLayout' layout = do
        -- master <- getImplementation
        mmsg   <- getMessage
        muser  <- maybeAuthId

        let loginR'     = AuthR loginR
            userMenu    = maybe $(widgetFile "loginForm")
                                (\user -> $(widgetFile "userLoggedIn"))
                                muser
        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.

        pc <- widgetToPageContent $ do
                setTitle "Welcome To HSync!"
                addScript $ StaticR js_jquery_min_js
                addScript $ StaticR js_bootstrap_min_js
                addScript $ StaticR js_bootstrap_confirmation_js

                $(combineStylesheets 'StaticR [ css_bootstrap_css --  css_normalize_css

                                              ])
                $(widgetFile "default-layout")
        withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

jumbotronLayout :: Widget -> Widget -> Handler Html
jumbotronLayout jumboContent mainContent = defaultLayout' $(widgetFile "jumbotron-layout")

-- How to access the stuff that we store using acid state

instance HasAcidState (HandlerT App IO) Realms where
  getAcidState = _serverRealms <$> getAcids

instance HasAcidState (HandlerT App IO) UserIndex where
  getAcidState = _userIndex <$> getAcids


instance YesodLocalAuth App where
  onRegister = createHomeRealm


createHomeRealm   :: User -> HandlerT App IO ()
createHomeRealm u = do dt <- currentTime
                       let lm = LastModified dt (u^.userId) webClientId
                       (ri,_,_) <- updateAcid $ CreateRealm rn lm ap
                       updateAcid . UpdateUser $ addRealm (realmRoot ri) u
  where
    rn  = FileName $ u^.userName.unUserName
    ap  = AccessPolicy $
          M.singleton (AccessUser (u^.userId)) (S.fromList [Read,Write])




instance YesodAuth App where
    type AuthId App = User

    -- Where to send a user after successful login
    loginDest _ = HomeR
    -- Where to send a user after logout
    logoutDest _ = HomeR
    -- Override the above two destinations when a Referer: header is present
    redirectToReferer _ = True



    authenticate creds = case validateUserName . credsIdent $ creds of
                           Left _   -> return $ UserError InvalidLogin
                           Right un -> do
                                         print un
                                         lookupUser un

    maybeAuthId = do
      m <- lookupSession credsKey
      return $ m >>= fromPathPiece

    -- You can add other plugins like BrowserID, email or OAuth here
    authPlugins _ = [localAuth]

    authHttpManager = getHttpManager



-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

unsafeHandler :: App -> Handler a -> IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger _appLogger




-- Note: Some functionality previously present in the scaffolding has been
-- moved to documentation in the Wiki. Following are some hopefully helpful
-- links:
--
-- https://github.com/yesodweb/yesod/wiki/Sending-email
-- https://github.com/yesodweb/yesod/wiki/Serve-static-files-from-a-separate-domain
-- https://github.com/yesodweb/yesod/wiki/i18n-messages-in-the-scaffolding
