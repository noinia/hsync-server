{-# LANGUAGE LambdaCase #-}
module HSync.Server.Handler.AcidUtils where

import Control.Lens
import qualified Data.Bimap as BM
import HSync.Server.Import
import HSync.Server.Notifications
import Data.Acid(UpdateEvent, EventResult, EventState)

--------------------------------------------------------------------------------
-- * Actual implementations for these things

addFile                        :: ClientId -> RealmId -> Path -> FileKind
                               -> Source Handler ByteString
                               -> Handler (Either ErrorMessage PublicNotification)
addFile ci ri p currentKind s = do
    (_,sig) <- storeFile ri p s
    elm     <- getLastModified ci
    case elm of
      Left err -> return $ Left err
      Right lm -> updateAcidAndLog $ AddFile ri p currentKind lm True sig


updateAcidAndLog     :: ( UpdateEvent event
                        , EventResult event ~ Either l (Notification ClientId)
                        , HasAcidState Handler (EventState event)
                        ) => event -> Handler (Either l PublicNotification)
updateAcidAndLog evt = do
                         n <- updateAcid evt
                         mapM_ logNotification n
                         pure n
                         -- mapM  withClientName n

getLastModified    :: ClientId -> Handler (Either ErrorMessage (LastModified ClientId))
getLastModified ci = do
    dt  <- currentTime
    mui <- (fmap (^.userId)) <$> maybeAuthId
    return $ case mui of
      Just ui -> Right $ LastModified dt ui ci
      Nothing -> Left "Error: We need a UserId to create a LastModified"

createDirectory                     :: ClientId -> RealmId -> Path -> FileKind
                                    -> Handler (Either ErrorMessage PublicNotification)
createDirectory ci ri p currentKind = getLastModified ci >>= \case
    Left err -> return $ Left err
    Right lm -> updateAcidAndLog $ AddDirectory ri p currentKind lm


deleteFileOrDir                     :: ClientId -> RealmId -> Path -> FileKind
                                    -> Handler (Either ErrorMessage PublicNotification)
deleteFileOrDir ci ri p currentKind = getLastModified ci >>= \case
    Left err -> return $ Left err
    Right lm -> updateAcidAndLog $ Delete ri p currentKind lm


withClientId      :: ClientName -> (ClientId -> Handler (Either ErrorMessage a))
                  -> Handler (Either ErrorMessage a)
withClientId cn h = queryClientId cn >>= \case
                      Nothing -> pure $ Left "Error: Unknown Client"
                      Just ci -> h ci


queryRealmName :: RealmId -> Handler (Maybe RealmName)
queryRealmName = fmap (fmap (^.realmName)) . queryAcid . QueryRealm

queryClientId    :: ClientName -> Handler (Maybe ClientId)
queryClientId cn = ((^.clients.to (BM.lookupR cn)) =<<) <$> maybeAuthId
