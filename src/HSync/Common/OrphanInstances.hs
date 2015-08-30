{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module HSync.Common.OrphanInstances where

import Prelude
import Data.Semigroup
import qualified Data.Bimap as BM
import qualified Data.List.NonEmpty as NE
import Data.SafeCopy
import Text.Blaze(Markup)

$(deriveSafeCopy 0 'base ''NE.NonEmpty)

instance Semigroup Markup where
  a <> b = a `mappend` b

-- instance Semigroup (Bimap a b) where
--   a <> b = B.toMap

-- instance Monoid (Bimap a b) where
--   mempty = BM.empty
--   a `mappend` b = a <> b


instance (Ord a, Ord b, Read a, Read b) => Read (BM.Bimap a b) where
  readsPrec i = map (\(a,s) -> (BM.fromAscPairListUnchecked a, s))
              . readsPrec i . drop (length ("fromList " :: String))


instance (Ord a, Ord b, SafeCopy a, SafeCopy b) => SafeCopy (BM.Bimap a b) where
  putCopy = putCopy . BM.toAscList
  getCopy = contain . fmap BM.fromAscPairListUnchecked $ safeGet
