{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
module HSync.Server.Application
    ( getApplicationDev
    , appMain
    , develMain
    , makeFoundation
    -- * for DevelMain
    , getApplicationRepl
    , shutdownApp
    -- * for GHCI
    , handler
    -- , db
    ) where

import Control.Lens
import Control.Monad.Logger                 (liftLoc)
import Control.Concurrent(forkIO)
import HSync.Server.Import
import Language.Haskell.TH.Syntax           (qLocation)
import Network.Wai.Handler.Warp             (Settings, defaultSettings,
                                             defaultShouldDisplayException,
                                             runSettings, setHost,
                                             setOnException, setPort, getPort)
import Network.Wai.Middleware.RequestLogger (Destination (Logger),
                                             IPAddrSource (..),
                                             OutputFormat (..), destination,
                                             mkRequestLogger, outputFormat)
import System.Log.FastLogger                (defaultBufSize, newStdoutLoggerSet,
                                             toLogStr)

import HSync.Server.AcidState
import HSync.Server.Notifications(printNotifications)

-- Import all relevant handler modules here.
-- Don't forget to add new modules to your cabal file!
import HSync.Server.Handler.AccessPolicy
import HSync.Server.Handler.Common
import HSync.Server.Handler.Home
import HSync.Server.Handler.ManualUpload
import HSync.Server.Handler.Realm
import HSync.Server.Handler.User
import HSync.Server.Handler.API

--------------------------------------------------------------------------------

instance YesodSubDispatch HSyncAPI (HandlerT App IO) where
  yesodSubDispatch = $(mkYesodSubDispatch resourcesHSyncAPI)

-- This line actually creates our YesodDispatch instance. It is the second half
-- of the call to mkYesodData which occurs in Foundation.hs. Please see the
-- comments there for more details.
mkYesodDispatch "App" resourcesApp



-- | This function allocates resources (such as a database connection pool),
-- performs initialization and return a foundation datatype value. This is also
-- the place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
makeFoundation :: AppSettings -> Acids -> IO App
makeFoundation _appSettings _appAcids = do
    -- Some basic initializations: HTTP connection manager, logger, and static
    -- subsite.
    _appHttpManager <- newManager
    _appLogger <- newStdoutLoggerSet defaultBufSize >>= makeYesodLogger
    _appStatic <-
        (if _appSettings^.mutableStatic then staticDevel else static)
        (_appSettings^.staticDir)
    _appNotificationChan <- newBroadcastTChanIO

    -- Return the foundation
    return $ App {..}

-- | Convert our foundation to a WAI Application by calling @toWaiAppPlain@ and
-- applyng some additional middlewares.
makeApplication :: App -> IO Application
makeApplication foundation = do
    logWare <- mkRequestLogger def
        { outputFormat =
            if foundation^.appSettings.detailedRequestLogging
                then Detailed True
                else Apache
                        (if foundation^.appSettings.ipFromHeader
                            then FromFallback
                            else FromSocket)
        , destination = Logger $ loggerSet $ foundation^.appLogger
        }

    -- Create the WAI application and apply middlewares
    appPlain <- toWaiAppPlain foundation
    return $ logWare $ defaultMiddlewaresNoLogging appPlain

-- | Warp settings for the given foundation value.
warpSettings :: App -> Settings
warpSettings foundation =
      setPort (foundation^.appSettings.port)
    $ setHost (foundation^.appSettings.host)
    $ setOnException (\_req e ->
        when (defaultShouldDisplayException e) $ messageLoggerSource
            foundation
            (foundation^.appLogger)
            $(qLocation >>= liftLoc)
            "yesod"
            LevelError
            (toLogStr $ "Exception from Warp: " ++ show e))
      defaultSettings

-- | For yesod devel, return the Warp settings and WAI Application.
getApplicationDev       :: Acids -> IO (Settings, Application)
getApplicationDev acids = do
    settings <- getAppSettings
    foundation <- makeFoundation settings acids
    _ <- forkIO (printNotifications foundation)
    wsettings <- getDevSettings $ warpSettings foundation
    app <- makeApplication foundation
    return (wsettings, app)

getAppSettings :: IO AppSettings
getAppSettings = loadAppSettings [configSettingsYml] [] useEnv

-- | main function for use by yesod devel
develMain :: IO ()
develMain = withAcids Nothing $ \acids -> do
  develMainHelper (getApplicationDev acids)

-- | The @main@ function for an executable running this site.
appMain :: IO ()
appMain = do
    -- Get the settings from all relevant sources
    settings <- loadAppSettingsArgs
        -- fall back to compile-time values, set to [] to require values at runtime
        [configSettingsYmlValue]

        -- allow environment variables to override
        useEnv

    withAcids (settings^.acidStatePath) $ \acids -> do
      -- Generate the foundation from the settings
      foundation <- makeFoundation settings acids

      -- Generate a WAI Application from the foundation
      app <- makeApplication foundation

      -- Run the application with Warp
      runSettings (warpSettings foundation) app


--------------------------------------------------------------
-- Functions for DevelMain.hs (a way to run the app from GHCi)
--------------------------------------------------------------
getApplicationRepl :: IO (Int, App, Application)
getApplicationRepl = do
    settings <- getAppSettings
    withAcids (settings^.acidStatePath) $ \acids -> do
      foundation <- makeFoundation settings acids
      wsettings <- getDevSettings $ warpSettings foundation
      app1 <- makeApplication foundation
      return (getPort wsettings, foundation, app1)

shutdownApp :: App -> IO ()
shutdownApp _ = return ()


---------------------------------------------
-- Functions for use in development with GHCi
---------------------------------------------

-- | Run a handler
handler :: Handler a -> IO a
handler h = do
    settings <- getAppSettings
    withAcids (settings^.acidStatePath) $ \acids -> do
      foundation <- makeFoundation settings acids
      unsafeHandler foundation h

-- | Run DB queries
-- db :: ReaderT SqlBackend (HandlerT App IO) a -> IO a
-- db = handler . runDB
