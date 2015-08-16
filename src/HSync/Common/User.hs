module HSync.Common.User where

import Data.Data(Typeable)
import ClassyPrelude.Yesod
import HSync.Common.Types
import HSync.Common.Realm
import HSync.Common.AccessPolicy
import Data.Set(Set)
import Data.SafeCopy(base, deriveSafeCopy)
import Text.Read(reads)
import Control.Lens
import qualified Data.Set as S


--------------------------------------------------------------------------------



-- data Client = Client { _clientId   :: ClientId
--                      , _clientName :: ClientName
--                      }
--               deriving (Show,Read,Eq,Ord,Typeable)
-- $(deriveSafeCopy 0 'base ''Client)
-- makeLenses ''Client


data User = User { _userId    :: UserId
                 , _userName  :: UserName
                 , _realName  :: RealName
                 , _password  :: HashedPassword
                 , _clients   :: Map ClientId ClientName
                 , _realms    :: Set (AccessPoint)
                 }
            deriving (Show,Read,Eq,Ord,Typeable)
$(deriveSafeCopy 0 'base ''User)
makeLenses ''User

-- Give a better instance here
instance PathPiece User where
  fromPathPiece t = case reads . unpack $ t of
                      [(u,"")] -> Just u
                      _        -> Nothing
  toPathPiece     = pack . show


addRealm      :: AccessPoint -> User -> User
addRealm ap u = u&realms %~ (S.insert ap)
