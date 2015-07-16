module HSync.Server.AcidState where

import Prelude
import Control.Monad.State.Class
import Control.Monad.Reader.Class
import HSync.Common.AcidState
import HSync.Common.Realm
import HSync.Common.Types
import HSync.Common.User(User)
import qualified HSync.Server.User as U

import qualified Data.Map as M

--------------------------------------------------------------------------------

import Data.Acid
    ( AcidState(..), EventState(..), EventResult(..)
    , Query(..), QueryEvent(..), Update(..), UpdateEvent(..)
    , IsAcidic(..), makeAcidic, openLocalState
    )



data Acids = Acids { _realms :: AcidState (M.Map RealmId Realm)
                   , _users  :: AcidState U.UserIndex
                   }


queryUserIndex :: Query U.UserIndex U.UserIndex
queryUserIndex = ask

lookupUser    :: UserName -> Query U.UserIndex (Maybe User)
lookupUser ui = U.lookupUser ui <$> ask

insertUser   :: User -> Update U.UserIndex (Maybe ErrorMessage)
insertUser u = do
                 eix <- U.insertUser u <$> get
                 case eix of
                   Left err -> return $ Just err
                   Right ix -> put ix >> return Nothing




$(makeAcidic ''U.UserIndex [ 'queryUserIndex
                           , 'lookupUser
                           , 'insertUser
                           ])
