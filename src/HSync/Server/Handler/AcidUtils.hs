{-# LANGUAGE LambdaCase #-}
module HSync.Server.Handler.AcidUtils where

import Control.Lens
import HSync.Server.Import


--------------------------------------------------------------------------------
-- * Actual implementations for these things

addFile                        :: ClientId -> RealmId -> Path -> FileKind
                               -> Source (ResourceT IO) ByteString
                               -> Handler (Either ErrorMessage FileVersion)
addFile ci ri p currentKind s = do
    (_,sig) <- storeFile ri p s
    elm     <- getLastModified ci
    case elm of
      Left err -> return $ Left err
      Right lm -> updateAcid $ AddFile ri p currentKind lm True sig

getLastModified    :: ClientId -> Handler (Either ErrorMessage LastModified)
getLastModified ci = do
    dt  <- currentTime
    mui <- (fmap (^.userId)) <$> maybeAuthId
    return $ case mui of
      Just ui -> Right $ LastModified dt ui ci
      Nothing -> Left "Error: We need a UserId to create a LastModified"

createDirectory                     :: ClientId -> RealmId -> Path -> FileKind
                                    -> Handler (Either ErrorMessage FileVersion)
createDirectory ci ri p currentKind = getLastModified ci >>= \case
    Left err -> return $ Left err
    Right lm -> updateAcid $ AddDirectory ri p currentKind lm


deleteFileOrDir                     :: ClientId -> RealmId -> Path -> FileKind
                                    -> Handler (Either ErrorMessage FileVersion)
deleteFileOrDir ci ri p currentKind = getLastModified ci >>= \case
    Left err -> return $ Left err
    Right lm -> updateAcid $ Delete ri p currentKind lm
