{-# OPTIONS_GHC -fno-warn-orphans #-}
module HSync.Server.User( UserIndex(..)
                          -- * Acididic Operations
                        , QueryUserIndex(..)
                        , LookupUserByName(..)
                        , InsertUser(..)
                        , UpdateUser(..)

                          -- * RegisterUser
                        , RegisterUser(..)

                          -- * Re-exports from Common.User:
                        , User(User), userId, userName, realName, password, clients, realms

                        , Client(Client), clientId, clientName

                        , addRealm
                        ) where

import qualified Data.Set as S

import ClassyPrelude.Yesod hiding (Update, Query, get)
import Control.Lens hiding (Indexable)
import HSync.Common.AcidState
import HSync.Common.Types                    as Import
import HSync.Common.User                     as Import
import Data.Default
import Data.IxSet
import Control.Monad.State.Class
import Control.Monad.Reader.Class
import qualified Data.IxSet as I
import Data.Acid(Query, Update, makeAcidic)
import Data.SafeCopy(base, deriveSafeCopy)


--------------------------------------------------------------------------------

instance Indexable User where
  empty = ixSet [ ixFun $ \u -> [ _userId u ]
                , ixFun $ \u -> [ _userName  u ]
                ]

--------------------------------------------------------------------------------

-- | Users have a unique userId and a unique username
data UserIndex = UserIndex { _userIxSet  :: IxSet User
                           , _nextUserId :: UserId
                           }
                  deriving (Show,Read,Eq)
$(deriveSafeCopy 0 'base ''UserIndex)
makeLenses ''UserIndex

instance Default UserIndex where
  def = UserIndex I.empty (UserId 1)


data RegisterUser = RegisterUser { reqUserName     :: UserName
                                 , reqRealName     :: RealName
                                 , reqHashedPasswd :: HashedPassword
                                 }
                    deriving (Show,Eq)
$(deriveSafeCopy 0 'base ''RegisterUser)



--------------------------------------------------------------------------------
-- * Acidic Operations

queryUserIndex :: Query UserIndex UserIndex
queryUserIndex = ask

lookupUserByName    :: UserName -> Query UserIndex (Maybe User)
lookupUserByName ui = lookupUserByName' ui <$> ask

insertUser    :: RegisterUser
              -> Update UserIndex (Either ErrorMessage (User,UserIndex))
insertUser ru = do
                  idx <- get
                  case I.toList $ (idx^.userIxSet) @= (reqUserName ru) of
                    [] -> let (u,idx') = insertUser' ru idx
                               in do put idx' ; return $ Right (u,idx')
                    _  -> return $ Left "Username already taken."


updateUser   :: User -> Update UserIndex ()
updateUser u = modify (&userIxSet %~ I.updateIx (u^.userId) u)


--------------------------------------------------------------------------------

insertUser' :: RegisterUser -> UserIndex -> (User,UserIndex)
insertUser' (RegisterUser n r hpw) (UserIndex idx ui@(UserId i)) =
  (u, UserIndex (I.insert u idx) (UserId $ i+1))
    where
      u = User ui n r hpw mempty mempty


lookupUserByName'                  :: UserName -> UserIndex -> Maybe User
lookupUserByName' ui (UserIndex s _) = I.getOne $ s @= ui

lookupUserById'                  :: UserId -> UserIndex -> Maybe User
lookupUserById' ui (UserIndex s _) = I.getOne $ s @= ui


--------------------------------------------------------------------------------

$(makeAcidic ''UserIndex [ 'queryUserIndex
                         , 'lookupUserByName
                         , 'insertUser
                         , 'updateUser
                         ])
