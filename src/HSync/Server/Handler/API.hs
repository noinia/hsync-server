module HSync.Server.Handler.API where

import Control.Lens
import HSync.Common.API
import HSync.Server.Import
import HSync.Common.Header
import HSync.Server.LocalAuth(validateUser)
import HSync.Server.Notifications
import HSync.Server.Handler.AcidUtils
import Data.Maybe(fromJust)
import Data.Aeson(encode)
import qualified Data.Conduit.List as C

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

getListenNowR      :: RealmId -> Path -> APIHandler TypedContent
getListenNowR ri p = lift $ respondWithSource (notificationsFor ri p)

getListenR        :: DateTime -> RealmId -> Path -> APIHandler TypedContent
getListenR d ri p = lift $ respondWithSource (notificationsAsOf d ri p)

-- | Given a function to produce a source of a's (that can be encoded as JSON).
-- Respond with this source
respondWithSource          :: ToJSON a
                           => Handler (Source Handler a) -> Handler TypedContent
respondWithSource mkSource = do
                               s <- mkSource
                               respondSource typePlain
                                 (s $= C.map encode $= awaitForever sendChunk')
    where
      sendChunk' x = sendChunk x >> sendFlush

--------------------------------------------------------------------------------

getCurrentRealmR      :: RealmId -> Path -> APIHandler Value
getCurrentRealmR ri p = do
                          mr <- lift . queryAcid $ Access ri p
                          case mr of
                            Nothing   -> notFound
                            Just node -> pure . toJSON . current' $ node

getDownloadR                  :: RealmId -> FileKind -> Path -> APIHandler TypedContent
getDownloadR _  NonExistent _ = notFound
getDownloadR _  Directory   _ = typedText_ ("Directory" :: Text)
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

postCreateDirR         :: ClientName -> RealmId -> Path -> APIHandler Value
postCreateDirR cn ri p = toJSON <$> lift (withClientId cn $ \ci ->
                                           createDirectory ci ri p NonExistent)


postStoreFileR             :: ClientName -> RealmId -> Signature -> Path
                           -> APIHandler Value
postStoreFileR cn ri sig p = toJSON <$> lift (withClientId cn $ \ci ->
                                          addFile ci ri p (File sig) rawRequestBody)


--------------------------------------------------------------------------------


deleteDeleteR                     :: ClientName -> RealmId -> FileKind -> Path
                                  -> APIHandler Value
deleteDeleteR _  _  NonExistent _ = reportError_ "Nothing to delete"
deleteDeleteR cn ri fk          p = toJSON <$> lift (withClientId cn $ \ci ->
                                                 deleteFileOrDir ci ri p fk)

reportError :: Text -> Either ErrorMessage FileVersion
reportError = Left

reportError_ :: Monad m => Text -> m Value
reportError_ = pure . toJSON . reportError

-- typedText
typedText :: Text -> TypedContent
typedText = toTypedContent

typedText_ :: Monad m => Text -> m TypedContent
typedText_ = return . typedText
