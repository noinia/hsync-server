{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module HSync.Common.FileVersion where

import ClassyPrelude.Yesod
import Control.Lens
import HSync.Common.Types
import Data.Semigroup
import HSync.Common.StorageTree(Measured(..))
import Data.SafeCopy

--------------------------------------------------------------------------------

newtype LastModificationTime = LastModificationTime { _unLMT :: Max UTCTime }
                               deriving (Show,Read,Eq,Ord,Semigroup)

instance SafeCopy LastModificationTime where
  putCopy (LastModificationTime (Max u)) = contain $ safePut u
  getCopy = contain $ LastModificationTime . Max <$> safeGet


data FileKind = Directory
              | File  { _dataPath  :: FilePath -- ^ Path where this file is actually stored
                      , _signature :: Signature
                      }
              | NonExistent
              deriving (Show,Read,Eq)
makePrisms ''FileKind
$(deriveSafeCopy 0 'base ''FileKind)

dataPath :: Traversal' FileKind FilePath
dataPath = _File._1

signature :: Traversal' FileKind Signature
signature = _File._2

data LastModified = LastModified { _modificationTime :: UTCTime
                                 , _modUser          :: UserId
                                 , _modClient        :: ClientId
                                 }
                    deriving (Show,Read,Eq)
makeLenses ''LastModified
$(deriveSafeCopy 0 'base ''LastModified)


instance Measured LastModificationTime LastModified where
  measure = LastModificationTime . Max . _modificationTime



data FileVersion = FileVersion { _fileKind      :: FileKind
                               , _lastModified  :: LastModified
                               , _dataCommitted :: Bool -- ^ Whether or not the data
                                                        --   has successfully been
                                                        --   written on disk (in case)
                                                        --   this is a file
                               }
                 deriving (Show,Read,Eq)
makeLenses ''FileVersion
$(deriveSafeCopy 0 'base ''FileVersion)


instance Measured LastModificationTime FileVersion where
  measure = measure . _lastModified
