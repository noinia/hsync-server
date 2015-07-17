{-# LANGUAGE FlexibleInstances #-}
module HSync.Server.Foundation where

import HSync.Server.Import.NoFoundation
import Text.Hamlet          (hamletFile)
import Text.Jasmine         (minifym)
import Yesod.Auth.Message   (AuthMessage (InvalidLogin))
import Yesod.Default.Util   (addStaticContentExternal)
import Yesod.Core.Types     (Logger)
import qualified Yesod.Core.Unsafe as Unsafe
import Control.Lens
import HSync.Common.Types
import HSync.Common.User
import HSync.Common.FileVersion
import HSync.Common.AccessPolicy
import HSync.Common.DateTime(currentTime)
import HSync.Server.User
import HSync.Server.Realm
import HSync.Common.Realm(realmRoot)
import HSync.Server.LocalAuth
import HSync.Server.AcidState
import HSync.Common.AcidState
import qualified Data.Map as M

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


getStatic = _appStatic

getAcids :: HandlerT App IO Acids
getAcids = _appAcids <$> getYesod


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

    defaultLayout mainContent = do
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
                $(combineStylesheets 'StaticR [ css_bootstrap_css --  css_normalize_css
                                              ])
                $(widgetFile "default-layout")
        withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")


    -- defaultLayout widget = do
    --     master <- getYesod
    --     mmsg <- getMessage

    --     -- We break up the default layout into two components:
    --     -- default-layout is the contents of the body tag, and
    --     -- default-layout-wrapper is the entire page. Since the final
    --     -- value passed to hamletToRepHtml cannot be a widget, this allows
    --     -- you to use normal widget features in default-layout.

    --     pc <- widgetToPageContent $ do
    --         addStylesheet $ StaticR css_bootstrap_css
    --         $(widgetFile "default-layout")
    --     withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

    -- The page to be redirected to when authentication is required.
    authRoute _ = Just $ AuthR LoginR

    -- Routes not requiring authentication.
    isAuthorized (AuthR _) _ = return Authorized
    isAuthorized FaviconR _ = return Authorized
    isAuthorized RobotsR _ = return Authorized
    -- Default to Authorized for now.
    isAuthorized _ _ = return Authorized

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent ext mime content = do
        master <- getYesod
        let staticDir = _appStaticDir $ _appSettings master
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
        app^.appSettings.appShouldLogAll
            || level == LevelWarn
            || level == LevelError

    makeLogger = return . _appLogger


-- How to access the stuff that we store using acid state

instance HasAcidState (HandlerT App IO) Realms where
  getAcidState = _serverRealms <$> getAcids

instance HasAcidState (HandlerT App IO) UserIndex where
  getAcidState = _users <$> getAcids


instance YesodLocalAuth App where
  onRegister = createHomeRealm


createHomeRealm   :: User -> HandlerT App IO ()
createHomeRealm u = do dt <- currentTime
                       let lm = LastModified dt (u^.userId) webClientId
                       (ri,_,_) <- updateAcid $ CreateRealm rn lm ap
                       updateAcid . UpdateUser $ addRealm (realmRoot ri) u
  where
    rn  = FileName $ u^.userName.unUserName
    ap  = [AccessUser $ M.singleton (u^.userId) [Read,Write]]

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
                           Right un -> lookupUser un

    maybeAuthId = do
      m <- lookupSession credsKey
      return $ m >>= fromPathPiece

    -- You can add other plugins like BrowserID, email or OAuth here
    authPlugins _ = [localAuth]

    authHttpManager = getHttpManager

lookupUser    :: UserName -> HandlerT App IO (AuthenticationResult App)
lookupUser un = maybe (UserError InvalidLogin) Authenticated
             <$> queryAcid (LookupUserByName un)

-- instance YesodAuthPersist App

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
