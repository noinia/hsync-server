{-# OPTIONS_GHC -fno-warn-orphans #-}
module HSync.Server.User where

import Prelude

import Data.Aeson.TH
import Data.Default
import Data.IxSet
import Data.SafeCopy(base, deriveSafeCopy)
import Data.Text(Text, pack, unpack)

import HSync.Common.Types
import HSync.Common.User
import Text.Blaze
import Yesod.Core

import qualified Data.IxSet as I

--------------------------------------------------------------------------------

-- newtype RealName = RealName { unRealName :: Text }
--                  deriving (Show,Read,Eq,Ord,Data,Typeable)
-- $(deriveSafeCopy 0 'base ''RealName)
-- $(deriveJSON defaultOptions ''RealName)

-- instance ToMarkup RealName where
--   toMarkup           = toMarkup           . unRealName
--   preEscapedToMarkup = preEscapedToMarkup . unRealName


-- instance ToMarkup UserIdent where
--   toMarkup           = toMarkup           . unUI
--   preEscapedToMarkup = preEscapedToMarkup . unUI


--------------------------------------------------------------------------------

-- data ServerUser = User { userId   :: UserIdent
--                        , realName :: RealName
--                        , password :: HashedPassword
--                        }
--                 deriving (Show,Read,Eq,Ord,Data,Typeable)

-- $(deriveSafeCopy 0 'base ''User)



instance Indexable User where
  empty = ixSet [ ixFun $ \u -> [ _userId u ]
                , ixFun $ \u -> [ _userName  u ]
                ]

--------------------------------------------------------------------------------

-- | Users have a unique userId
newtype UserIndex = UserIndex { unUIdx :: IxSet User }
                  deriving (Show,Read,Eq)
$(deriveSafeCopy 0 'base ''UserIndex)

instance Default UserIndex where
  def = UserIndex I.empty


insertUser                    :: User -> UserIndex -> Either ErrorMessage UserIndex
insertUser u ui@(UserIndex s) = case lookupUser (_userName u) ui of
  Nothing -> Right $ UserIndex (I.insert u s)
  Just _  -> Left    "Username already taken."

lookupUser                  :: UserName -> UserIndex -> Maybe User
lookupUser ui (UserIndex s) = I.getOne $ s @= ui
