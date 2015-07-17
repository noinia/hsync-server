module HSync.Server.AcidState where


import Prelude
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans(MonadIO(..))
import Data.Default
import HSync.Common.AcidState
import HSync.Common.Types
import HSync.Server.User
import HSync.Server.Realm
import Data.Acid(AcidState(..))
import Control.Lens

--------------------------------------------------------------------------------




data Acids = Acids { _serverRealms :: AcidState Realms
                   , _users        :: AcidState UserIndex
                   }
makeLenses ''Acids

-- instance Default Acids where
--   def = Acids def def


withAcids            :: ( MonadBaseControl IO m
                        , MonadIO m
                        ) => Maybe FilePath -> (Acids -> m a) -> m a
withAcids stateDir f = withAcidState stateDir def $ \realms ->
                         withAcidState stateDir def $ \users ->
                           f (Acids realms users)
