module HSync.Server.Handler.API where

import HSync.Common.API
import HSync.Server.Import


--------------------------------------------------------------------------------

postAPILoginR :: APIHandler ()
postAPILoginR = undefined


--------------------------------------------------------------------------------

getListenNowR :: RealmId -> Path -> APIHandler TypedContent
getListenNowR ri p = return . toTypedContent $ ("Woei" :: Text)

getListenR :: DateTime -> RealmId -> Path -> APIHandler TypedContent
getListenR = undefined

--------------------------------------------------------------------------------

getRealmR :: RealmId -> Path -> APIHandler TypedContent
getRealmR ri p = undefined


getDownloadR                  :: RealmId -> FileKind -> Path -> APIHandler TypedContent
getDownloadR _  NonExistent _ = return $ toTypedContent ("NonExistent file" :: Text)
getDownloadR ri Directory   p = return $ toTypedContent ("Directory" :: Text)
getDownloadR ri (File s)    p = getFileR ri s p


getFileR        :: RealmId -> Signature -> Path -> APIHandler TypedContent
getFileR ri s p = do
                    fp <- lift $ getFilePath ri p s
                    sendFile (contentTypeOf p) fp

getLatestFileR      :: RealmId -> Path -> APIHandler TypedContent
getLatestFileR ri p = undefined --- return ()




--------------------------------------------------------------------------------

postCreateDirR         :: ClientId -> RealmId -> Path -> APIHandler TypedContent
postCreateDirR ci ri p = undefined

postStoreFileR           :: ClientId -> RealmId -> Signature -> Path
                         -> APIHandler TypedContent
postStoreFileR ci ri s p = undefined


--------------------------------------------------------------------------------

deleteDeleteDirR :: ClientId -> RealmId -> Path -> APIHandler TypedContent
deleteDeleteDirR ci ri p = undefined

deleteDeleteFileR :: ClientId -> RealmId -> Signature -> Path -> APIHandler TypedContent
deleteDeleteFileR ci ri s p = undefined
