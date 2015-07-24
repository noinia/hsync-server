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
    { _staticDir              :: String
    -- ^ Directory from which to serve static files.
    , _appRoot                   :: Text
    -- ^ Base for all generated URLs.
    , _host                   :: HostPreference
    -- ^ Host/interface the server should bind to.
    , _port                   :: Int
    -- ^ Port to listen on
    , _ipFromHeader           :: Bool
    -- ^ Get the IP address from the header when logging. Useful when sitting
    -- behind a reverse proxy.

    , _acidStatePath          :: Maybe FilePath
    -- ^ Path with acid state stuff. If Nothing use the default ('state')

    , _detailedRequestLogging :: Bool
    -- ^ Use detailed request logging system
    , _shouldLogAll           :: Bool
    -- ^ Should all log messages be displayed?
    , _reloadTemplates        :: Bool
    -- ^ Use the reload version of templates
    , _mutableStatic          :: Bool
    -- ^ Assume that files in the static dir may change after compilation
    , _skipCombining          :: Bool
    -- ^ Perform no stylesheet/script combining


    , _filesPath              :: FilePath
    -- ^ Path where we store the actual files

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
        _staticDir              <- o .: "static-dir"
        -- appDatabaseConf           <- o .: "database"
        _appRoot                <- o .: "approot"
        _host                   <- fromString <$> o .: "host"
        _port                   <- o .: "port"
        _ipFromHeader           <- o .: "ip-from-header"
        _acidStatePath          <- o .:? "acid-state-path"

        _detailedRequestLogging <- o .:? "detailed-logging" .!= defaultDev
        _shouldLogAll           <- o .:? "should-log-all"   .!= defaultDev
        _reloadTemplates        <- o .:? "reload-templates" .!= defaultDev
        _mutableStatic          <- o .:? "mutable-static"   .!= defaultDev
        _skipCombining          <- o .:? "skip-combining"   .!= defaultDev
        _filesPath              <- o .: "files-path"

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
widgetFile = (if compileTimeAppSettings^.reloadTemplates
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
    (_skipCombining compileTimeAppSettings)
    combineSettings

combineScripts :: Name -> [Route Static] -> Q Exp
combineScripts = combineScripts'
    (_skipCombining compileTimeAppSettings)
    combineSettings


-- | ClientName and ID to use if we change something something from the website.
webClientName :: ClientName
webClientName = ClientName "web"

webClientId :: ClientId
webClientId = ClientId 0
