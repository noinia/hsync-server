-- | Settings are centralized, as much as possible, into this file. This
-- includes database connection settings, static file locations, etc.
-- In addition, you can configure a number of different aspects of Yesod
-- by overriding methods in the Yesod typeclass. That instance is
-- declared in the Foundation.hs file.
module HSync.Server.Settings where

import HSync.Common.Types
import Control.Lens
import ClassyPrelude.Yesod
import Control.Exception           (throw)
import Data.Aeson                  (Result (..), fromJSON, withObject, (.!=),
                                    (.:?))
import Data.FileEmbed              (embedFile)
import Data.Yaml                   (decodeEither')
-- import Database.Persist.Sqlite     (SqliteConf)
import Language.Haskell.TH.Syntax  (Exp, Name, Q)
import Network.Wai.Handler.Warp    (HostPreference)
import Yesod.Default.Config2       (applyEnvValue, configSettingsYml)
import Yesod.Default.Util          (WidgetFileSettings, widgetFileNoReload,
                                    widgetFileReload)

-- | Runtime settings to configure this application. These settings can be
-- loaded from various sources: defaults, environment variables, config files,
-- theoretically even a database.
data AppSettings = AppSettings
    { _appStaticDir              :: String
    -- ^ Directory from which to serve static files.
    -- , appDatabaseConf           :: SqliteConf
    -- ^ Configuration settings for accessing the database.
    , _appRoot                   :: Text
    -- ^ Base for all generated URLs.
    , _appHost                   :: HostPreference
    -- ^ Host/interface the server should bind to.
    , _appPort                   :: Int
    -- ^ Port to listen on
    , _appIpFromHeader           :: Bool
    -- ^ Get the IP address from the header when logging. Useful when sitting
    -- behind a reverse proxy.

      -- Path with acid state stuff
    , _appAcidStatePath          :: Maybe FilePath


    , _appDetailedRequestLogging :: Bool
    -- ^ Use detailed request logging system
    , _appShouldLogAll           :: Bool
    -- ^ Should all log messages be displayed?
    , _appReloadTemplates        :: Bool
    -- ^ Use the reload version of templates
    , _appMutableStatic          :: Bool
    -- ^ Assume that files in the static dir may change after compilation
    , _appSkipCombining          :: Bool
    -- ^ Perform no stylesheet/script combining
    }
makeLenses ''AppSettings

instance FromJSON AppSettings where
    parseJSON = withObject "AppSettings" $ \o -> do
        let defaultDev =
#if DEVELOPMENT
                True
#else
                False
#endif
        _appStaticDir              <- o .: "static-dir"
        -- appDatabaseConf           <- o .: "database"
        _appRoot                   <- o .: "approot"
        _appHost                   <- fromString <$> o .: "host"
        _appPort                   <- o .: "port"
        _appIpFromHeader           <- o .: "ip-from-header"
        _appAcidStatePath          <- o .:? "acid-state-path"

        _appDetailedRequestLogging <- o .:? "detailed-logging" .!= defaultDev
        _appShouldLogAll           <- o .:? "should-log-all"   .!= defaultDev
        _appReloadTemplates        <- o .:? "reload-templates" .!= defaultDev
        _appMutableStatic          <- o .:? "mutable-static"   .!= defaultDev
        _appSkipCombining          <- o .:? "skip-combining"   .!= defaultDev

        return AppSettings {..}

-- | Settings for 'widgetFile', such as which template languages to support and
-- default Hamlet settings.
--
-- For more information on modifying behavior, see:
--
-- https://github.com/yesodweb/yesod/wiki/Overriding-widgetFile
widgetFileSettings :: WidgetFileSettings
widgetFileSettings = def

-- | How static files should be combined.
combineSettings :: CombineSettings
combineSettings = def

-- The rest of this file contains settings which rarely need changing by a
-- user.

widgetFile :: String -> Q Exp
widgetFile = (if compileTimeAppSettings^.appReloadTemplates
                then widgetFileReload
                else widgetFileNoReload)
              widgetFileSettings

-- | Raw bytes at compile time of @config/settings.yml@
configSettingsYmlBS :: ByteString
configSettingsYmlBS = $(embedFile configSettingsYml)

-- | @config/settings.yml@, parsed to a @Value@.
configSettingsYmlValue :: Value
configSettingsYmlValue = either throw id $ decodeEither' configSettingsYmlBS

-- | A version of @AppSettings@ parsed at compile time from @config/settings.yml@.
compileTimeAppSettings :: AppSettings
compileTimeAppSettings =
    case fromJSON $ applyEnvValue False mempty configSettingsYmlValue of
        Error e -> error e
        Success settings -> settings

-- The following two functions can be used to combine multiple CSS or JS files
-- at compile time to decrease the number of http requests.
-- Sample usage (inside a Widget):
--
-- > $(combineStylesheets 'StaticR [style1_css, style2_css])

combineStylesheets :: Name -> [Route Static] -> Q Exp
combineStylesheets = combineStylesheets'
    (_appSkipCombining compileTimeAppSettings)
    combineSettings

combineScripts :: Name -> [Route Static] -> Q Exp
combineScripts = combineScripts'
    (_appSkipCombining compileTimeAppSettings)
    combineSettings


-- | ClientName and ID to use if we change something something from the website.
webClientName :: ClientName
webClientName = ClientName "web"

webClientId :: ClientId
webClientId = ClientId 0
