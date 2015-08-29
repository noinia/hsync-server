{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module HSync.Common.FileVersion where

import Data.Aeson.TH
import ClassyPrelude.Yesod
import Control.Lens
import HSync.Common.Types
import Data.Semigroup
import HSync.Common.StorageTree(Measured(..))
import Data.SafeCopy
import HSync.Common.DateTime
import qualified Data.Text as T
import Text.Blaze(ToMarkup(..))

--------------------------------------------------------------------------------

newtype LastModificationTime = LastModificationTime { _unLMT :: Max DateTime }
                               deriving (Show,Read,Eq,Ord,Semigroup)
makeLenses ''LastModificationTime

instance SafeCopy LastModificationTime where
  putCopy (LastModificationTime (Max u)) = contain $ safePut u
  getCopy = contain $ LastModificationTime . Max <$> safeGet

instance ToMarkup LastModificationTime where
  toMarkup = toMarkup . getMax . _unLMT

instance ToJSON LastModificationTime where
  toJSON = toJSON . getMax . _unLMT

instance FromJSON LastModificationTime where
  parseJSON = fmap (LastModificationTime . Max) . parseJSON




--------------------------------------------------------------------------------

data FileKind = Directory
              | File Signature
              | NonExistent
              deriving (Show,Read,Eq)
makePrisms ''FileKind
$(deriveSafeCopy 0 'base ''FileKind)
$(deriveJSON defaultOptions ''FileKind)




instance PathPiece FileKind where
  toPathPiece Directory   = "Directory"
  toPathPiece (File s)    = "File_" <> toPathPiece s
  toPathPiece NonExistent = "NonExistent"

  fromPathPiece "Directory" = pure Directory
  fromPathPiece "NonExistent" = pure NonExistent
  fromPathPiece t = case splitAt 5 t of
      ("File_",t') -> File <$> fromPathPiece t'
      _            -> Nothing

isFile          :: FileKind -> Bool
isFile (File _) = True
isFile _        = False

isDirectory           :: FileKind -> Bool
isDirectory Directory = True
isDirectory _         = False

exists             :: FileKind -> Bool
exists NonExistent = False
exists _           = True


signature :: Traversal' FileKind Signature
signature = _File


--------------------------------------------------------------------------------

data LastModified = LastModified { _modificationTime :: DateTime
                                 , _modUser          :: UserId
                                 , _modClient        :: ClientId
                                 }
                    deriving (Show,Read,Eq)
makeLenses ''LastModified
$(deriveSafeCopy 0 'base ''LastModified)
$(deriveJSON defaultOptions ''LastModified)


instance Measured LastModificationTime LastModified where
  measure = LastModificationTime . Max . _modificationTime


--------------------------------------------------------------------------------


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
$(deriveJSON defaultOptions ''FileVersion)


instance Measured LastModificationTime FileVersion where
  measure = measure . _lastModified
