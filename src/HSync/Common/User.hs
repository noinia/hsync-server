module HSync.Common.User where

import Data.Data(Data,Typeable)
import ClassyPrelude.Yesod
import HSync.Common.Types
import HSync.Common.Realm
import HSync.Common.AccessPolicy
import Data.Set(Set)
import Data.SafeCopy(base, deriveSafeCopy)
import Text.Read(reads)

--------------------------------------------------------------------------------



data Client = Client { _clientId   :: ClientId
                     , _clientName :: ClientName
                     }
              deriving (Show,Read,Eq,Ord,Typeable)
$(deriveSafeCopy 0 'base ''Client)


data User = User { _userId   :: UserId
                 , _userName :: UserName
                 , _realName :: RealName
                 , password  :: HashedPassword
                 , _clients  :: Set (Client)
                 , _realms   :: Set (AccessPoint)
                 }
            deriving (Show,Read,Eq,Ord,Typeable)
$(deriveSafeCopy 0 'base ''User)


-- Give a better instance here
instance PathPiece User where
  fromPathPiece t = case reads . unpack $ t of
                      [(u,"")] -> Just u
                      _        -> Nothing
  toPathPiece     = pack . show
