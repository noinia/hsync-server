{-# LANGUAGE LambdaCase #-}
module HSync.Server.Handler.ViewTree where

import Control.Lens hiding (children)
import HSync.Server.Import
import HSync.Server.Handler.ManualUpload(webCreateDir, webStoreFile)
import HSync.Common.StorageTree hiding (updateAt, access, Path)

import qualified Data.Map as M

--------------------------------------------------------------------------------

getViewTreeR      :: RealmId -> Path -> Handler Html
getViewTreeR ri p@(Path ps) = do
                      mr <- queryAcid $ Access ri p
                      case mr of
                        Nothing   -> do setMessage "No such Realm or Path"
                                        notFound
                        Just node -> do
                          let curNode = current' node
                              chs     = fmap (^.unOrdByName) . toList $ curNode^.children
                          createDirWidget <- webCreateDir ri p
                          storeFileWidget <- webStoreFile ri p
                          jumbotronLayout $(widgetFile "viewNodeData")
                                          $(widgetFile "viewChildren")
  where
    pathOf c = Path $ ps <> [c^.name]

    fileKindIcon c = fileKindIcon' $ c^.nodeData.fileKind


fileKindIcon' NonExistent = Nothing
fileKindIcon' (File _ _)  = Just "glyphicon-file"
fileKindIcon' Directory   = Just "glyphicon-folder-open"


getListRealmsR :: Handler Html
getListRealmsR = do
                   (Realms m n) <- queryAcid QueryRealms
                   let realms = M.assocs m
                   defaultLayout $(widgetFile "listRealms")
