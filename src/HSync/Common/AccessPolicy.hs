module HSync.Common.AccessPolicy where

import Prelude
import HSync.Common.Types
import qualified Data.Map as M
import Data.SafeCopy(base, deriveSafeCopy)
import qualified Data.Set as S
--------------------------------------------------------------------------------

data AccessRight = Read | Write
                 deriving (Show,Read,Eq,Ord)
$(deriveSafeCopy 0 'base ''AccessRight)



data AccessOption = AccessUser     (M.Map UserId (S.Set AccessRight))
                  | AccessHash     HashedURL     (S.Set AccessRight)
                  | AccessPassword Password      (S.Set AccessRight)
                  deriving (Show,Read,Eq)
$(deriveSafeCopy 0 'base ''AccessOption)


type AccessPolicy = [AccessOption]
