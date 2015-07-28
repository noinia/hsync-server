module HSync.Server.Types where


import HSync.Server.Import.NoFoundation

--------------------------------------------------------------------------------

data AccessOptionType = ByAnonimous | ByPassword | ByUser
                      deriving (Show,Read,Eq,Ord)


instance PathPiece AccessOptionType where
  toPathPiece   = toPathPieceShow
  fromPathPiece = fromPathPieceRead
