module HSync.Server.Handler.FileActions where

import           HSync.Server.Import


import           Data.Aeson(encode)
import           Control.Monad.Trans.Resource(runResourceT, ResourceT)
import           Data.Conduit
import           Data.Conduit.Binary
import qualified Data.Conduit.List as C
import           Data.ByteString(ByteString)
import           Network.Wai(requestBody)
import           HSync.Common.TimedFSTree(MTimeTree, readMTimeTree)
import           HSync.Common.Header
import           HSync.Common.Notification
import           HSync.Server.Notifications( logNotification
                                           , notificationsFor
                                           , notificationsAsOf
                                           )
import           System.Directory( removeFile , createDirectory , doesDirectoryExist )


--------------------------------------------------------------------------------
-- * Handles related to notifications


-- | Get all notifications for path p as of time dt. It is assumed that dt lies in the
-- past.
getListenR      :: DateTime -> Path -> Handler TypedContent
getListenR dt p = respondWithSource (notificationsAsOf dt p)

-- | Get all notifications for path p, starting *now*
getListenNowR   :: Path -> Handler TypedContent
getListenNowR p = respondWithSource (notificationsFor p)

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
-- * Handles related to listing files/trees

-- | Produces a JSON value representing a Maybe MTimeTree
getTreeR :: Path -> Handler Value
getTreeR = fmap toJSON . getTreeOf


-- | Given a path, get the tree (if it exists) with modification times.
getTreeOf   :: Path -> Handler (Maybe MTimeTree)
getTreeOf p = asLocalPath p >>= \fp ->
                liftIO $ protect (doesDirectoryExist fp)
                                 (readMTimeTree fp) -- TODO: Should I create a lock here?
                                 (return Nothing)

--------------------------------------------------------------------------------
-- * Handles related to file events

-- | Serve a file
getFileR   :: Path -> Handler TypedContent
getFileR p = serveFile p


-- | Handler that allows uploading a new file to replace the old file. The
-- handler checks that the specified FileIdent matches the file we currently
-- have. Only if the FileIdents match, the file is replaced.
postPutFileR                 :: FileIdent -> Path -> Handler Text
postPutFileR (Directory _) _ = invalidArgs ["putFile: cannot replace directory by a file."]
postPutFileR fi            p = do
                                 bodySource' <- requestBody <$> waiRequest
                                 let bodySource = transPipe liftIO bodySource'
                                 -- transPipe lifts the underlying monad of
                                 -- bodySource' that is, the IO monad, into
                                 -- something more general; i.e. MonadIO m
                                 ci <- clientId
                                 atomicallyWriteR p $ putFile ci fi p bodySource

-- | See `postPutFileR`.
putFile           :: ClientIdent -- ^ ClientId of the client putting the file
                  -> FileIdent -- ^ old file ident
                  -> Path      -- ^ path to put the file to
                  -> Source (ResourceT IO) ByteString -- ^ The file contents
                  -> FilePath  -- ^ filepath corresponding to fp
                  -> Handler (Either ErrorDescription Notification)
putFile ci fi p s fp = protectedByFI fi fp "putFile" $ do
                           liftIO . runResourceT $ s $$ sinkFile fp
                           addFIHeader p
                           notification ci (determineEvent fi p)
  where
      determineEvent NonExistent = fileAdded
      determineEvent (File _)    = flip fileUpdated fi
      determineEvent _           = error "putFile: unknown event."


-- | Delete the file at the given path (provided that the FileIdent of that
-- file matches the given fileIdent).
deleteDeleteR      :: FileIdent -> Path -> Handler Text
deleteDeleteR fi p = atomicallyWriteR p delete'
  where
      delete' fp = protectedByFI fi fp "delete" $ do
                        liftIO $ removeFile fp -- removeFile is atomic anyway
                        dt <- currentTime
                        addDeletionHeader dt
                        ci <- clientId
                        return $ Notification (fileRemoved p fi) ci dt

--------------------------------------------------------------------------------
-- * Patching and Deltas. I.e. send only the changes in the file rather than the
--  complete file.

getDeltaR   :: Path -> Handler TypedContent
getDeltaR _ = serveSource dummy


getSignatureR   :: Path -> Handler TypedContent
getSignatureR _ = serveSource dummy


postPatchR                :: FileIdent -> Path -> Handler Text
postPatchR NonExistent _ = invalidArgs ["postPatch: cannot patch a nonexistent file."]
postPatchR _           p = atomicallyWriteR p patch'
    where
      patch' = error "postPatch: unimplemented"

--------------------------------------------------------------------------------
-- * Handles related to directoryevents

-- | Create a new directory.
postPutDirR     :: FileIdent -> Path -> Handler Text
postPutDirR fi p = clientId >>= postPutDirR' fi p


-- | Implementation of `postPutDirR`
postPutDirR'         :: FileIdent   -- ^ Current fileIdent.
                     -> Path        -- ^ path at which to create the directory.
                     -> ClientIdent -- ^ ClientIdent that is creating the directory
                     -> Handler Text
postPutDirR' fi p ci = asLocalPath p >>= (withNotification . putDir)
  where
    putDir fp = protectedByFI fi fp "putDir" $ do
                          liftIO $ createDirectory fp
                          addFIHeader p
                          notification ci (directoryAdded p)

--------------------------------------------------------------------------------
-- | Helper functions

-- | Given a source, stream it as a handler.
serveSource   :: Source Handler ByteString -> Handler TypedContent
serveSource s = respondSource typeOctet (s $= awaitForever sendChunkBS)

-- | Serve a file (makding sure to put the FileIdent of the file in one of the
-- headers)
serveFile   :: Path -> Handler a
serveFile p = addFIHeader p >> asLocalPath p >>= sendFile typeOctet


-- | Sets a 'hFileIdent' header with the new fileIdent
addFIHeader   :: Path -> Handler ()
addFIHeader p = getFileIdent p >>= addTypedHeader HFileIdent

-- | Adds a deletion time header
addDeletionHeader :: DateTime -> Handler ()
addDeletionHeader = addTypedHeader HDeletionTime


-- | Get the FileIdent for a file.
getFileIdent   :: Path -> Handler FileIdent
getFileIdent p = asLocalPath p >>= fileIdent


type FINotification = Either ErrorDescription Notification

-- TODO: Use the hName somewhere
-- | Runs the given handler atomically.
atomicallyWriteR              :: Path -> (FilePath -> Handler FINotification)
                              -> Handler Text
atomicallyWriteR p h = asLocalPath p >>= \fp -> withNotification (h' fp)
    where
      h' fp = atomicallyWriteIO fp (h fp)

-- | Runs a handler h that should proce a notification, and logs the
-- notification.
withNotification   :: Handler FINotification -> Handler Text
withNotification h = h >>= \mn -> case mn of
                       Left err -> invalidArgs err
                       Right n  -> logNotification n >> return "OK"


-- | Produce a notification with the given arguments and the current data/time
notification       :: ClientIdent -> Event -> Handler Notification
notification ci evt = (Notification evt ci) <$> currentTime



-- | Retireve the clientId from the request information
clientId :: Handler ClientIdent
clientId = lookupTypedHeader HClientId >>=
             maybe (invalidArgs ["clientId: Invalid clientId."])
                   return




--------------------------------------------------------------------------------
-- | Testing functions


dummy :: MonadResource m => Source m ByteString
dummy = sourceFile "/Users/frank/tmp/test.jpg"
