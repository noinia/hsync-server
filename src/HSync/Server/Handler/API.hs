module HSync.Server.Handler.API where

import Control.Lens
import HSync.Common.API
import HSync.Server.Import
import HSync.Common.Header
import HSync.Server.LocalAuth(validateUser)
import HSync.Server.Handler.AcidUtils
import Data.Maybe(fromJust)

--------------------------------------------------------------------------------

postAPILoginR :: APIHandler Value
postAPILoginR = lift $ do
    mu    <-                       lookupTypedHeader HUserName
    mpw   <- fmap hashPassword <$> lookupTypedHeader HPassword
    b <- validateUser' mu mpw
    when b $ setCreds False $ Creds "PostAPILoginR" ((fromJust mu)^.unUserName) []
    return $ toJSON b
  where
    validateUser' mu mpw = fromMaybe (pure False) $ validateUser <$> mu <*> mpw


--------------------------------------------------------------------------------

getListenNowR :: RealmId -> Path -> APIHandler TypedContent
getListenNowR ri p = return . toTypedContent $ ("Woei" :: Text)

getListenR :: DateTime -> RealmId -> Path -> APIHandler TypedContent
getListenR = undefined

--------------------------------------------------------------------------------

getCurrentRealmR      :: RealmId -> Path -> APIHandler Value
getCurrentRealmR ri p = do
                          mr <- lift . queryAcid $ Access ri p
                          case mr of
                            Nothing   -> notFound
                            Just node -> pure . toJSON . current' $ node

getDownloadR                  :: RealmId -> FileKind -> Path -> APIHandler TypedContent
getDownloadR _  NonExistent _ = notFound
getDownloadR ri Directory   p = typedText_ ("Directory" :: Text)
getDownloadR ri (File s)    p = getFileR ri s p


getFileR        :: RealmId -> Signature -> Path -> APIHandler TypedContent
getFileR ri s p = do
                    fp <- lift $ getFilePath ri p s
                    sendFile (contentTypeOf p) fp

getDownloadCurrentR      :: RealmId -> Path -> APIHandler TypedContent
getDownloadCurrentR ri p = do
                             mr <- lift . queryAcid $ Access ri p
                             case (^.nodeData.headVersionLens.fileKind) <$> mr of
                               Nothing  -> notFound
                               Just fk  -> getDownloadR ri fk p


--------------------------------------------------------------------------------

postCreateDirR         :: ClientId -> RealmId -> Path -> APIHandler Value
postCreateDirR ci ri p = toJSON <$> lift (createDirectory ci ri p NonExistent)


postStoreFileR             :: ClientId -> RealmId -> Signature -> Path
                           -> APIHandler Value
postStoreFileR ci ri sig p = toJSON <$> lift (addFile ci ri p (File sig) rawRequestBody)

--------------------------------------------------------------------------------


deleteDeleteR                     :: ClientId -> RealmId -> FileKind -> Path
                                  -> APIHandler Value
deleteDeleteR _  _  NonExistent _ = reportError_ "Nothing to delete"
deleteDeleteR ci ri fk          p = toJSON <$> lift (deleteFileOrDir ci ri p fk)


reportError :: Text -> Either ErrorMessage FileVersion
reportError = Left

reportError_ :: Monad m => Text -> m Value
reportError_ = pure . toJSON . reportError

-- typedText
typedText :: Text -> TypedContent
typedText = toTypedContent

typedText_ :: Monad m => Text -> m TypedContent
typedText_ = return . typedText
