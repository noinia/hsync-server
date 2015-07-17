{-# OPTIONS_GHC -fno-warn-orphans #-}
module HSync.Common.OrphanInstances where

import Prelude
import qualified Data.List.NonEmpty as NE
import Data.SafeCopy(base, deriveSafeCopy)

$(deriveSafeCopy 0 'base ''NE.NonEmpty)
