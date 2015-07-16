module HSync.Common.Types where

import Data.Char(isAlphaNum)
import ClassyPrelude.Yesod
import Data.SafeCopy(base, deriveSafeCopy)
import Text.Blaze(ToMarkup(..))
import Data.SafeCopy(SafeCopy(..))

import qualified Crypto.Hash
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as B
-- import qualified Data.ByteString as B

-- import Prelude
-- import Data.Text(Text)
-- import Data.ByteString(ByteString)
-- import Data.String(IsString(..))

--------------------------------------------------------------------------------

type ErrorMessage = Text

--------------------------------------------------------------------------------

newtype FileName = FileName Text
                   deriving (Show,Read,Eq,Ord,IsString,ToMarkup,PathPiece)
$(deriveSafeCopy 0 'base ''FileName)


newtype Path = Path [FileName]
               deriving (Show,Read,Eq,Ord)
$(deriveSafeCopy 0 'base ''Path)

instance PathMultiPiece Path where
    fromPathMultiPiece xs      = Path <$> mapM fromPathPiece xs
    toPathMultiPiece (Path fs) = map toPathPiece fs

--------------------------------------------------------------------------------


newtype ClientId = ClientId Integer
                   deriving (Show,Read,Eq,Ord,ToMarkup)
$(deriveSafeCopy 0 'base ''ClientId)

newtype ClientName = ClientName Text
                      deriving (Show,Read,Eq,Ord,ToMarkup)
$(deriveSafeCopy 0 'base ''ClientName)


--------------------------------------------------------------------------------


newtype RealmId = RealmId Integer
                deriving (Show,Read,Eq,Ord,ToMarkup)
$(deriveSafeCopy 0 'base ''RealmId)

newtype RealmName = RealmName Text
                  deriving (Show,Read,Eq,Ord,ToMarkup)
$(deriveSafeCopy 0 'base ''RealmName)

--------------------------------------------------------------------------------

--------------------------------------------------------------------------------


newtype UserId = UserId { _unUserId :: Integer }
                 deriving (Show,Read,Eq,Ord,ToMarkup)
$(deriveSafeCopy 0 'base ''UserId)



newtype UserName = UserName { _unUserName :: Text  }
                      deriving (Show,Read,Eq,Ord,ToMarkup)
$(deriveSafeCopy 0 'base ''UserName)

instance PathPiece UserName where
  toPathPiece   = _unUserName
  fromPathPiece = either (const Nothing) Just . userName


userName   :: Text -> Either ErrorMessage UserName
userName t
  | T.all userNameChar t = Right $ UserName t
  | otherwise            = Left "Invalid UserName. Only alphanumeric characters allowed."

userNameChar = isAlphaNum


newtype RealName = RealName { _unRealName :: Text }
                 deriving (Show,Read,Eq,Ord,ToMarkup)
$(deriveSafeCopy 0 'base ''RealName)
-- $(deriveJSON defaultOptions ''RealName)

--------------------------------------------------------------------------------

newtype Password = Password { unPassword :: Text }
                    deriving (Show,Read,Eq,Ord,
                              PathPiece,FromJSON,ToJSON)
$(deriveSafeCopy 0 'base ''Password)



newtype HashedPassword = HashedPassword { unHashedPassword :: Text }
                    deriving (Show,Read,Eq,Ord,
                              PathPiece,FromJSON,ToJSON)
$(deriveSafeCopy 0 'base ''HashedPassword)


sha1 :: ByteString -> Crypto.Hash.Digest Crypto.Hash.SHA1
sha1 = Crypto.Hash.hash

hash' :: Text -> Text
hash' = T.pack . show . sha1 . B.pack . T.unpack

hashedPassword :: Password -> HashedPassword
hashedPassword = HashedPassword . hash' . unPassword

--------------------------------------------------------------------------------

newtype HashedURL = HashedURL { _hashedUrlData :: ByteString }
                    deriving (Show,Read,Eq,Ord)
$(deriveSafeCopy 0 'base ''HashedURL)


newtype Signature = Signature { _signatureData :: ByteString }
                    deriving (Show,Read,Eq,Ord)
$(deriveSafeCopy 0 'base ''Signature)
