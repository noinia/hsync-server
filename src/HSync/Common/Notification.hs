module HSync.Common.Notification(-- * Events
                                  EventKind(..)
                                , _Added, _Updated, _Deleted

                                , mkEventKind

                                -- , involvesFile, involvesDirectory
                                , Event(..)
                                , eventKind, newVersion, affectedPath

                                -- , fileAdded , fileRemoved, fileUpdated
                                -- , directoryAdded, directoryRemoved

                                -- * Notifications
                                , Notification(..), event
                                , notification


                                , toLog
                                , matchesNotification
                                ) where

import ClassyPrelude.Yesod

import Control.Lens

import Data.ByteString(ByteString)

import Data.Aeson.TH
import Data.SafeCopy(base, deriveSafeCopy)

import HSync.Common.Types
import HSync.Common.DateTime(DateTime)
import HSync.Common.FileVersion

import qualified Data.ByteString.Char8 as B

--------------------------------------------------------------------------------

data EventKind = Added
               | Updated FileVersion
               | Deleted FileVersion
               deriving (Show,Read,Eq)
$(deriveJSON defaultOptions ''EventKind)
$(deriveSafeCopy 0 'base ''EventKind)
makePrisms ''EventKind



mkEventKind                :: Maybe FileVersion -> FileVersion -> EventKind
mkEventKind Nothing    _   = Added
mkEventKind (Just old) new = case new^.fileKind of
                               NonExistent -> Deleted old
                               _           -> Updated old

data Event = Event { _eventKind        :: EventKind
                   , _newVersion       :: FileVersion
                   , _affectedRealm    :: RealmId
                   , _affectedPath     :: Path
                   }
             deriving (Show,Read,Eq)
$(deriveJSON defaultOptions ''Event)
$(deriveSafeCopy 0 'base ''Event)
makeLenses ''Event

-- fileAdded        :: Path -> Event
-- fileRemoved      :: Path -> FileIdent -> Event
-- fileUpdated      :: Path -> FileIdent -> Event

-- fileAdded   p    = Event FileAdded p   NonExistent
-- fileRemoved p fi = Event FileRemoved p fi
-- fileUpdated p fi = Event FileUpdated p fi

-- directoryAdded        :: Path -> Event
-- directoryRemoved      :: Path -> FileIdent -> Event


-- directoryAdded   p    = Event DirectoryAdded p   NonExistent
-- directoryRemoved p fi = Event DirectoryRemoved p fi


--------------------------------------------------------------------------------

newtype Notification = Notification { _event :: Event }
                  deriving (Read,Eq,Show)
makeLenses ''Notification
$(deriveJSON defaultOptions ''Notification)
$(deriveSafeCopy 0 'base ''Notification)


-- | smart constructor to construct a notification
notification              :: Maybe FileVersion -> FileVersion -> RealmId -> Path
                          -> Notification
notification old new ri p = Notification $ Event (mkEventKind old new) new ri p


-- | Notifications are ordered on timestamp
instance Ord Notification where
  compare = compare `on` (^.event.newVersion.lastModified.modificationTime)

toLog   :: Notification -> String
toLog n = show n

  -- let lm = n^.event.newVersion.lastModified
  --         in


  -- intercalate ":" [ show $ lm^.modificationTime
  --                 , show $ lm^.modClient
  --                 , show $ n^.eventKind
  --                 ]

  -- intercalate ":" $ [show ti, show ci, show evt]

fromLog :: ByteString -> Maybe Notification
fromLog = const Nothing --TODO: Implement this


-- | Weather or not the notification is about something from the subtree with
-- path p in realm ri .
matchesNotification        :: RealmId -> Path -> Notification -> Bool
matchesNotification ri p n =  n^.event.affectedRealm == ri
                           && p `isSubPathOf` (n^.event.affectedPath)



-- evtS = "FileAdded (Path \"\" [])"

-- testS = B.pack "2013-08-23-18:40:59.975000000000-UTC:\"\":FileAdded (Path \"\" [])"
