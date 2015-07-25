{-# LANGUAGE LambdaCase #-}
module HSync.Server.Handler.Realm where

import Control.Lens hiding (children)
import HSync.Server.Import
import HSync.Server.Handler.ManualUpload(webCreateDir, webStoreFile)
import HSync.Common.StorageTree hiding (updateAt, access, Path)

import qualified Data.Map as M
import qualified Data.List.NonEmpty as NE

--------------------------------------------------------------------------------

getViewRealmR                :: RealmId -> Path -> Handler Html
getViewRealmR ri p@(Path ps) = do
                      mr <- queryAcid $ Access ri p
                      case mr of
                        Nothing   -> do setMessage "No such Realm or Path"
                                        notFound
                        Just node -> do
                          let chs     = fmap (^.unOrdByName) . toList $ node^.children
                          createDirWidget <- webCreateDir ri p
                          storeFileWidget <- webStoreFile ri p
                          jumbotronLayout $(widgetFile "viewNodeData")
                                          $(widgetFile "viewChildren")
  where
    pathOf c = Path $ ps <> [c^.name]
    fileKindOf c = c^.nodeData.headVersionLens.fileKind
    isNonExistent = not . exists . fileKindOf


fileIcon :: Html
fileIcon = [shamlet|<span class="glyphicon glyphicon-file" aria-hidden="true">|]

directoryIcon :: Html
directoryIcon = [shamlet|<span class="glyphicon glyphicon-folder-open" aria-hidden="true">|]


iconOf      :: RealmTree -> Html
iconOf node = case   NE.dropWhile (not . exists)
                   . fmap (^.fileKind)             $ node^.nodeData.versions of
    ((File _) : _)   -> fileIcon
    (Directory : _) -> directoryIcon
    _               -> fileIcon -- this case should not occur actually


getListRealmsR :: Handler Html
getListRealmsR = do
                   (Realms m _) <- queryAcid QueryRealms
                   let allRealms = M.assocs m
                   defaultLayout $(widgetFile "listRealms")
