module HSync.Common.AccessPolicy where

import Prelude
import HSync.Common.Types
import qualified Data.Map as M
import Data.SafeCopy(base, deriveSafeCopy)

--------------------------------------------------------------------------------

data AccessRight = Read | Write
                 deriving (Show,Read,Eq,Ord)
$(deriveSafeCopy 0 'base ''AccessRight)



data AccessOption = AccessUser     (M.Map UserId [AccessRight])
                  | AccessHash     HashedURL     [AccessRight]
                  | AccessPassword Password      [AccessRight]
                  deriving (Show,Read,Eq)
$(deriveSafeCopy 0 'base ''AccessOption)


type AccessPolicy = [AccessOption]
