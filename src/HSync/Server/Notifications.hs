module HSync.Server.Notifications where

import           Control.Lens
import           HSync.Server.Import
import qualified System.Log.FastLogger as FL
import           Data.Conduit.List as CL

--------------------------------------------------------------------------------
-- | Storing Notifications


-- | Log the given notification
logNotification   :: Notification -> Handler ()
logNotification n = do
                       c <- (^.appNotificationChan) <$> getYesod
                       lift $ atomically (writeTChan c n)

printNotificationSink    :: MonadIO m => FL.LoggerSet -> Sink Notification m ()
printNotificationSink ls = awaitForever $ printNotification
  where
    printNotification :: MonadIO m => Notification -> m ()
    printNotification = liftIO . FL.pushLogStr ls . FL.toLogStr . (<> "\n") . toLog



--------------------------------------------------------------------------------
-- | Obtaining Notifications

-- | Get a stream of notifications in the Handler monad
notifications :: Handler (Source Handler Notification)
notifications = getYesod >>= notifications'

-- | Get a stream of notifications, starting *now*, for path p
notificationsFor      :: RealmId -> Path -> Handler (Source Handler Notification)
notificationsFor ri p = ($= CL.filter (matchesNotification ri p)) <$> notifications

-- | Get a stream of notifications for path p as of dt
notificationsAsOf        :: DateTime -> RealmId -> Path
                         -> Handler (Source Handler Notification)
notificationsAsOf dt ri p = do
                              newNots <- notificationsFor ri p
                              oldNots <- loadNotificationsAsOf dt ri p
                              return $ oldNots `concatSources` newNots

-- | Get a stream of notifications
notifications' :: MonadIO m
               => App -> m (Source m Notification)
notifications' = fmap chanToSource . dupChan' . (^.appNotificationChan)
  where
    dupChan' c = liftIO $ atomically (dupTChan c)


chanToSource   :: MonadIO m => TChan a -> Source m a
chanToSource c = do
                   x <- liftIO $ atomically (readTChan c)
                   yield x
                   chanToSource c

--------------------------------------------------------------------------------

-- | Print a log entry to a file for each notification
printNotifications     :: MonadIO m => App -> m ()
printNotifications hss = do
  let fp = "logs/notificationLog.log"
        -- hss^.appSettings.
        -- extraNotificationLog . appExtra . settings $ hss
  ls <- liftIO $ FL.newFileLoggerSet FL.defaultBufSize fp
  notifications' hss >>= ($$ printNotificationSink ls)


-- Given two sources s1 and s2 generate a source that *first* streams everything
-- from s1. If s1 is done only then start producing results using s2
concatSources       :: Monad m => Source m a -> Source m a -> Source m a
concatSources s1 s2 = s1 >> s2


-- | Gives a source with all notifications, *up until now* in the tree.
loadNotificationsAsOf      :: Monad m
                           => DateTime -> RealmId -> Path
                           -> Handler (Source m Notification)
loadNotificationsAsOf dt ri = fmap sourceList . loadNotificationsAsOfList dt ri

loadNotificationsAsOfList        :: DateTime -> RealmId -> Path -> Handler [Notification]
loadNotificationsAsOfList dt ri p = queryAcid $ NotificationsAsOf dt ri p
