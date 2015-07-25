{-# LANGUAGE LambdaCase #-}
module HSync.Server.Handler.Realm where

import Control.Lens hiding (children)
import HSync.Server.Import
import HSync.Server.Handler.ManualUpload(webCreateDir, webStoreFile)

import qualified Data.Map as M
import qualified Data.List.NonEmpty as NE

--------------------------------------------------------------------------------

-- TODO: improve this thing
getViewRealmR      :: RealmId -> Path -> Handler Html
getViewRealmR ri p = do
                      mr <- queryAcid $ Access ri p
                      case mr of
                        Nothing   -> do setMessage "No such Realm or Path"
                                        notFound
                        Just node -> getViewRealmR' ri p node


getViewRealmR'           :: RealmId -> Path -> RealmTree -> Handler Html
getViewRealmR' ri p node = do
    firstW   <- renderVersion x
    jumbotronLayout $(widgetFile "viewNodeData")
                    $(widgetFile "nodeVersions")
  where
    allVersions = node^.nodeData.versions
    (x NE.:| _) = withPrevAndNext allVersions

    renderVersion (mRecent,cur,mPrev) = getViewNodeVersion ri p node cur



withPrevAndNext    :: NE.NonEmpty a -> NE.NonEmpty (Maybe a, a, Maybe a)
withPrevAndNext xs = NE.fromList $ zip3 prevs xs' nexts
  where
    xs'   = NE.toList xs
    prevs = Nothing : map Just xs'
    nexts = map Just (NE.tail xs) ++ [Nothing]

getViewNodeVersion                       :: RealmId -> Path -> RealmTree -> FileVersion
                                         -> Handler Widget
getViewNodeVersion ri p@(Path ps) node v = case v^.fileKind of
    NonExistent -> return $(widgetFile "nonExistent")
    Directory   -> do
                     createDirWidget <- webCreateDir ri p
                     storeFileWidget <- webStoreFile ri p
                     return $(widgetFile "viewDirectory")
    File sig    -> return $(widgetFile "viewFile")
  where
    lastModifiedWidget = let user         = v^.lastModified.modUser
                             client       = v^.lastModified.modClient
                             modifiedTime = v^.lastModified.modificationTime
                         in $(widgetFile "lastModified")

    chs                = fmap (^.unOrdByName) . toList $ node^.children

    pathOf     c = Path $ ps <> [c^.name]
    fileKindOf c = c^.nodeData.headVersionLens.fileKind

    isNonExistent = not . exists . fileKindOf





fileIcon :: Html
fileIcon = [shamlet|<span class="glyphicon glyphicon-file" aria-hidden="true">|]

directoryIcon :: Html
directoryIcon = [shamlet|<span class="glyphicon glyphicon-folder-open" aria-hidden="true">|]

iconOf      :: RealmTree -> Html
iconOf node = case (^.fileKind) <$> lastExistingVersion (node^.nodeData) of
    Just (File _)  -> fileIcon
    Just Directory -> directoryIcon
    _              -> fileIcon -- this case should not occur actually


getListRealmsR :: Handler Html
getListRealmsR = do
                   (Realms m _) <- queryAcid QueryRealms
                   let allRealms = M.assocs m
                   defaultLayout $(widgetFile "listRealms")
