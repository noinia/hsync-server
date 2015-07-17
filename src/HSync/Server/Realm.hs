module HSync.Server.Realm where

import Data.Monoid
import Data.Default
import Prelude
import HSync.Common.Realm
import HSync.Common.Types
import HSync.Common.AccessPolicy
import HSync.Common.FileVersion
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



createRealm           :: RealmName -> LastModified -> AccessPolicy
                      -> Update Realms (RealmId,Realm,Realms)
createRealm rn lm pol = do
                       Realms m ni@(RealmId i) <- get
                       let r  = Realm (newRealmTree rn lm) pol
                       let rs = Realms (M.insert ni r m) (RealmId $ i+1)
                       put rs
                       return (ni,r,rs)


updateRealm     :: RealmId -> Realm -> Update Realms ()
updateRealm i r = modify (&realmMap %~ (M.insert i r))

queryRealm   :: RealmId -> Query Realms (Maybe Realm)
queryRealm i = M.lookup i . _realmMap <$> ask



$(makeAcidic ''Realms [ 'createRealm
                      , 'updateRealm
                      , 'queryRealm
                      ])
