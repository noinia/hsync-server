module HSync.Server.Realm where

import Data.Monoid
import Data.Default
import Prelude
import HSync.Common.AcidState
import HSync.Common.Realm
import HSync.Common.Types
import Control.Monad.State.Class
import Control.Monad.Reader.Class
import Data.Acid( Query(..), Update(..), makeAcidic)
import Data.SafeCopy(base, deriveSafeCopy)
import Control.Lens hiding (Indexable)
import qualified Data.Map as M

--------------------------------------------------------------------------------

data Realms = Realms { _realmMap    :: M.Map RealmId Realm
                     , _nextRealmId :: RealmId
                     }
              deriving (Show,Read)
$(deriveSafeCopy 0 'base ''Realms)
makeLenses ''Realms

instance Default Realms where
  def = Realms mempty (RealmId 0)



createRealm :: Update Realms (Realm,Realms)
createRealm = undefined


$(makeAcidic ''Realms [ 'createRealm
                         -- , 'lookupUserByName
                        -- , 'insertUser
                      ])
