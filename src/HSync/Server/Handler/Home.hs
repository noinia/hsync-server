module HSync.Server.Handler.Home where

import Control.Lens
import HSync.Server.Import
import HSync.Server.Handler.Realm(getViewRealmR)
import qualified Data.Set as S

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getHomeR :: Handler Html
getHomeR = maybe getAnonymousHomeR f =<< maybeAuthId
  where
    f u = case listToMaybe . S.toList $ u^.realms of
            Nothing                 -> getAnonymousHomeR
            Just (AccessPoint ri p) -> getViewRealmR ri p

getAnonymousHomeR :: Handler Html
getAnonymousHomeR = getAboutR

getAboutR :: Handler Html
getAboutR = do
    -- (formWidget, formEnctype) <- generateFormPost sampleForm
    -- let submission = Nothing :: Maybe (FileInfo, Text)
    --     handlerName = "getHomeR" :: Text
    jumbotronLayout $(widgetFile "homepage")
                    $(widgetFile "about")
