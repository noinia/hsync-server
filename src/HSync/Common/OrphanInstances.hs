{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module HSync.Common.OrphanInstances where

import Data.Semigroup
import qualified Data.List.NonEmpty as NE
import Data.SafeCopy(base, deriveSafeCopy)
import Text.Blaze(Markup)

$(deriveSafeCopy 0 'base ''NE.NonEmpty)

instance Semigroup Markup where
  a <> b = a `mappend` b
