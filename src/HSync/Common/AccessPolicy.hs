{-# LANGUAGE FlexibleInstances #-}
module HSync.Common.AccessPolicy where

import ClassyPrelude.Yesod
import Control.Lens
import HSync.Common.Types
import HSync.Common.Util
import HSync.Common.OrphanInstances()
import qualified Data.Map as M
import Data.SafeCopy(base, deriveSafeCopy)
import qualified Data.Set as S
import qualified Data.Text as T
import Text.Blaze(ToMarkup(..))

--------------------------------------------------------------------------------

data AccessRight = Read | Write
                 deriving (Show,Read,Eq,Ord)
$(deriveSafeCopy 0 'base ''AccessRight)

instance ToMarkup AccessRight where
  toMarkup = toMarkup . show

instance PathPiece AccessRight where
  toPathPiece   = toPathPieceShow
  fromPathPiece = fromPathPieceRead


data AccessOption = AccessAnonymous
                  | AccessPassword   HashedPassword
                  | AccessUser       UserId
                  deriving (Show,Read,Eq,Ord)
$(deriveSafeCopy 0 'base ''AccessOption)
makePrisms ''AccessOption

instance ToMarkup AccessOption where
  toMarkup AccessAnonymous     = "AccessAnonymous"
  toMarkup (AccessPassword pw) = "AccessPassword " <> toMarkup pw
  toMarkup (AccessUser ui)     = "AccessUser" <> toMarkup ui


instance PathPiece AccessOption where
  toPathPiece AccessAnonymous     = "Anonymous"
  toPathPiece (AccessPassword pw) = "Password_" <> toPathPiece pw
  toPathPiece (AccessUser ui)     = "User_" <> toPathPiece ui

  fromPathPiece "Anonymous" = Just AccessAnonymous
  fromPathPiece t = case (T.stripPrefix "Password_" t, T.stripPrefix "User_" t) of
    (Just t', _) -> AccessPassword <$> fromPathPiece t'
    (_, Just t') -> AccessUser     <$> fromPathPiece t'
    _            -> Nothing



data AccessItem = AccessItem { _accessOption :: AccessOption
                             , _accessRights :: S.Set AccessRight
                             }
                  deriving (Show,Read,Eq,Ord)
$(deriveSafeCopy 0 'base ''AccessItem)
makeLenses ''AccessItem

instance PathPiece (S.Set AccessRight) where
  toPathPiece   = T.intercalate "," . map toPathPiece . toList
  fromPathPiece = fmap S.fromList . mapM fromPathPiece . T.splitOn ","

instance PathPiece AccessItem where
  toPathPiece (AccessItem o r) = "AccessItem_" <> toPathPiece o
                               <> "_Rights_" <> toPathPiece r
  fromPathPiece t = f =<< (T.splitOn "_Rights_" <$> T.stripPrefix "AccessItem_" t)
    where
      f [x,y] = AccessItem <$> fromPathPiece x <*> fromPathPiece y
      f _     = Nothing


newtype AccessPolicy = AccessPolicy {
                         _accessOptions :: M.Map AccessOption (S.Set AccessRight) }
                     deriving (Show,Read,Eq,Ord,Semigroup,Monoid)
$(deriveSafeCopy 0 'base ''AccessPolicy)
makeLenses ''AccessPolicy


lookupAccessRights   :: AccessOption -> AccessPolicy -> S.Set AccessRight
lookupAccessRights k = fromMaybe mempty . M.lookup k . _accessOptions
