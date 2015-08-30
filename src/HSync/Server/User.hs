{-# OPTIONS_GHC -fno-warn-orphans #-}
module HSync.Server.User( UserIndex(..)

                          -- * Acididic Operations
                        , QueryUserIndex(..)
                        , LookupUserByName(..)
                        , LookupUserById(..)
                        , InsertUser(..)
                        , UpdateUser(..)
                        , AddClient(..)


                          -- * RegisterUser
                        , RegisterUser(..)


                          -- * Re-exports from Common.User:
                        , User(User), userId, userName, realName, password, clients, realms

                        , addRealm
                        ) where

import qualified Data.Set as S

import ClassyPrelude.Yesod hiding (Update, Query, get)
import Control.Lens hiding (Indexable)
import HSync.Common.AcidState
import HSync.Common.Types
import HSync.Common.User
import Data.IxSet
import Control.Monad.State.Class
import Control.Monad.Reader.Class
import qualified Data.IxSet as I
import qualified Data.Bimap as BM
import qualified Data.Map   as M
import Data.Acid(Query, Update, makeAcidic, liftQuery)
import Data.SafeCopy(base, deriveSafeCopy)


--------------------------------------------------------------------------------

instance Indexable User where
  empty = ixSet [ ixFun $ \u -> [ _userId u ]
                , ixFun $ \u -> [ _userName  u ]
                ]

--------------------------------------------------------------------------------

-- | Users have a unique userId and a unique username
data UserIndex = UserIndex { _userIxSet    :: IxSet User
                           , _nextUserId   :: UserId
                           , _nextClientId :: Map UserId ClientId
                           }
                  deriving (Show,Read,Eq)
$(deriveSafeCopy 0 'base ''UserIndex)
makeLenses ''UserIndex

instance Default UserIndex where
  def = UserIndex I.empty (UserId 1) mempty


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

lookupUserById    :: UserId -> Query UserIndex (Maybe User)
lookupUserById ui = lookupUserById' ui <$> ask


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


-- Registers that we have a new client
addClient      :: UserName -> ClientName -> Update UserIndex (Maybe ClientId)
addClient un c = liftQuery (lookupUserByName un) >>= \case
                   Nothing -> pure Nothing
                   Just u  -> do ci <- takeNextClientId (u^.userId)
                                 updateUser $ u&clients %~ BM.insert ci c
                                 pure $ Just ci

takeNextClientId    :: UserId -> Update UserIndex ClientId
takeNextClientId ui = do
                        ci <- use (nextClientId.at ui.non (ClientId 1))
                        modify (&nextClientId.at ui ?~ succ ci)
                        pure ci

--------------------------------------------------------------------------------

insertUser' :: RegisterUser -> UserIndex -> (User,UserIndex)
insertUser' (RegisterUser n r hpw) (UserIndex idx ui ncm) =
  (u, UserIndex (I.insert u idx) (succ ui) ncm)
    where
      u = User ui n r hpw BM.empty mempty


lookupUserByName'                      :: UserName -> UserIndex -> Maybe User
lookupUserByName' ui (UserIndex s _ _) = I.getOne $ s @= ui

lookupUserById'                      :: UserId -> UserIndex -> Maybe User
lookupUserById' ui (UserIndex s _ _) = I.getOne $ s @= ui


--------------------------------------------------------------------------------

$(makeAcidic ''UserIndex [ 'queryUserIndex
                         , 'lookupUserByName
                         , 'lookupUserById
                         , 'insertUser
                         , 'updateUser

                         , 'addClient
                         ])
