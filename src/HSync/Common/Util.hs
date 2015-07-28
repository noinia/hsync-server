module HSync.Common.Util where

import ClassyPrelude.Yesod
import qualified Data.Text as T
import Text.Read(reads)

--------------------------------------------------------------------------------

toPathPieceShow :: Show a => a -> Text
toPathPieceShow = T.pack . show


fromPathPieceRead   :: Read a => Text -> Maybe a
fromPathPieceRead t = case reads . T.unpack $ t of
                        ((x,""):_) -> Just x
                        _          -> Nothing
