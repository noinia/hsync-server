{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
module HSync.Server.Handler.Realm where

import Control.Lens hiding (children)
import HSync.Server.Import
import HSync.Server.Handler.ManualUpload(webCreateDir, webStoreFile)
import HSync.Server.Handler.AcidUtils(queryRealmName)

import qualified Data.Bimap as BM
import qualified Data.Map as M
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T

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
    rn <- queryRealmName ri
    allRenders <- mapM (\(i,v) -> (i,v,) <$> getViewNodeVersion ri p node v)
               $ NE.zip (NE.fromList [1..]) allVersions
    jumbotronLayout $(widgetFile "viewNodeData")
                    $(widgetFile "nodeVersions")
  where
    allVersions@(latestVersion NE.:| _) = node^.nodeData.versions
    numVersions = NE.length allVersions

    fk = latestVersion^.fileKind

    isOne i = i == 1

    parentPath = let ps = map (^.unFileName) $ (parentOf p)^.pathParts
                 in "/" <> T.intercalate "/" ps



-- | get a specific version with the given last mod. time
getViewRealmVersionR         :: RealmId -> DateTime -> Path -> Handler Html
getViewRealmVersionR ri dt p = undefined


withPrevAndNext    :: NE.NonEmpty a -> NE.NonEmpty (Maybe a, a, Maybe a)
withPrevAndNext xs = NE.fromList $ zip3 prevs xs' nexts
  where
    xs'   = NE.toList xs
    prevs = Nothing : map Just xs'
    nexts = map Just (NE.tail xs) ++ [Nothing]

getViewNodeVersion                       :: RealmId -> Path -> RealmTree -> FileVersion ClientId
                                         -> Handler Widget
getViewNodeVersion ri p@(Path ps) node v = do
    lastModifiedWidget <- getLastModifiedWidget v
    case v^.fileKind of
      NonExistent -> return $(widgetFile "nonExistent")
      Directory   -> do
                       showAddWidgets     <- (&& isLatest)
                                         <$> hasRights [Write] Nothing ri p
                       createDirWidget    <- webCreateDir ri p
                       storeFileWidget    <- webStoreFile ri p
                       return $(widgetFile "viewDirectory")
      File sig    -> return $(widgetFile "viewFile")
  where
    chs                = fmap (^.unOrdByName) . toList $ node^.children

    pathOf     c = Path $ ps <> [c^.name]
    fileKindOf c = c^.nodeData.headVersionLens.fileKind

    isNonExistent = not . exists . fileKindOf

    isLatest = node^.nodeData.headVersionLens == v

getLastModifiedWidget   :: FileVersion ClientId -> Handler Widget
getLastModifiedWidget v = do
    user <- queryAcid . LookupUserById $ v^.lastModified.modUser
    let clientId     = v^.lastModified.modClient
        modifiedTime = v^.lastModified.modificationTime
        userName'    = user^._Just.userName.unUserName
        clientName   = if clientId == webClientId
                          then webClientName
                          else webClientName -- TODO!!!!
                         -- user^?_Just.clients.to (BM.lookup clientId)
          -- webClientName -- fromMaybe webClientName $ user^._Just.clients.at clientId
    return $(widgetFile "lastModified")




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
