module HSync.Server.Handler.API where

import HSync.Common.API
import HSync.Server.Import
import HSync.Server.Handler.AcidUtils


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
getDownloadR _  NonExistent _ = typedText_ ("NonExistent file" :: Text)
getDownloadR ri Directory   p = typedText_ ("Directory" :: Text)
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


postStoreFileR             :: ClientId -> RealmId -> Signature -> Path
                           -> APIHandler TypedContent
postStoreFileR ci ri sig p = do
                               er <- lift $ addFile ci ri p (File sig) rawRequestBody
                               case er of
                                 Left err -> typedText_ err
                                 Right _  -> typedText_ "OK"

--------------------------------------------------------------------------------


deleteDeleteR                     :: ClientId -> RealmId -> FileKind -> Path
                                  -> APIHandler TypedContent
deleteDeleteR _  _  NonExistent _ = return . toTypedContent $ ()
deleteDeleteR ci ri fk          p = do
                                      er <- lift $ deleteFileOrDir ci ri p fk
                                      case er of
                                        Left err -> typedText_ err
                                        Right _  -> typedText_ "OK"


-- typedText
typedText :: Text -> TypedContent
typedText = toTypedContent

typedText_ :: Monad m => Text -> m TypedContent
typedText_ = return . typedText
