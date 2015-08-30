module HSync.Common.Types where

import Data.Aeson.TH
import Data.Char(isAlphaNum)
import ClassyPrelude.Yesod
import Data.SafeCopy(base, deriveSafeCopy)
import Text.Blaze(ToMarkup(..))
import Data.SafeCopy(SafeCopy(..))
import Control.Lens
import qualified Crypto.Hash as Hash
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as B
import qualified Data.List as L

import Data.Serialize(encode,decode)
import Crypto.Conduit(hashFile)
import qualified Crypto.Hash.CryptoAPI as CryptoAPI

--------------------------------------------------------------------------------

type ErrorMessage = Text

--------------------------------------------------------------------------------

newtype FileName = FileName { _unFileName :: Text }
                   deriving (Show,Read,Eq,Ord,IsString,ToMarkup,PathPiece,Typeable
                            ,ToJSON,FromJSON)
$(deriveSafeCopy 0 'base ''FileName)
makeLenses ''FileName

newtype Path = Path { _pathParts :: [FileName] }
               deriving (Show,Read,Eq,Ord,Typeable)
$(deriveSafeCopy 0 'base ''Path)
$(deriveJSON defaultOptions ''Path)
makeLenses ''Path

-- | Parent of the root is the root itself
parentOf           :: Path -> Path
parentOf (Path []) = Path []
parentOf (Path p)  = Path . L.init $ p


isSubPathOf :: Path -> Path -> Bool
(Path p) `isSubPathOf` (Path q) = p `isPrefixOf` q

instance PathMultiPiece Path where
    fromPathMultiPiece xs      = Path <$> mapM fromPathPiece xs
    toPathMultiPiece (Path fs) = map toPathPiece fs

instance ToMarkup Path where
  toMarkup = mconcat . L.intersperse slash . map toMarkup . _pathParts
    where
      slash = toMarkup ("/" :: Text)

--------------------------------------------------------------------------------


newtype ClientId = ClientId { _unClientId :: Integer }
                   deriving ( Show,Read,Eq,Ord,ToMarkup,Enum
                            , Typeable,PathPiece,ToJSON,FromJSON)
$(deriveSafeCopy 0 'base ''ClientId)
makeLenses ''ClientId

newtype ClientName = ClientName { _unClientName :: Text }
                      deriving (Show,Read,Eq,Ord,ToMarkup,Typeable,ToJSON,FromJSON,PathPiece)
$(deriveSafeCopy 0 'base ''ClientName)
makeLenses ''ClientName

--------------------------------------------------------------------------------


newtype RealmId = RealmId Integer
                deriving ( Show,Read,Eq,Ord,Enum
                         , Typeable,ToMarkup,PathPiece,FromJSON,ToJSON)
$(deriveSafeCopy 0 'base ''RealmId)

type RealmName = FileName



--------------------------------------------------------------------------------

--------------------------------------------------------------------------------


newtype UserId = UserId { _unUserId :: Integer }
                 deriving ( Show,Read,Eq,Ord,Enum
                          , ToMarkup,Typeable,FromJSON,ToJSON,PathPiece)
$(deriveSafeCopy 0 'base ''UserId)
makeLenses ''UserId

newtype UserName = UserName { _unUserName :: Text  }
                      deriving (Show,Read,Eq,Ord,ToMarkup,Typeable,FromJSON,ToJSON)
$(deriveSafeCopy 0 'base ''UserName)
makeLenses ''UserName

instance PathPiece UserName where
  toPathPiece   = _unUserName
  fromPathPiece = either (const Nothing) Just . validateUserName


validateUserName   :: Text -> Either ErrorMessage UserName
validateUserName t
  | T.all userNameChar t = Right $ UserName t
  | otherwise            = Left "Invalid UserName. Only alphanumeric characters allowed."

userNameChar :: Char -> Bool
userNameChar = isAlphaNum


newtype RealName = RealName { _unRealName :: Text }
                 deriving (Show,Read,Eq,Ord,ToMarkup,Typeable,FromJSON,ToJSON)
$(deriveSafeCopy 0 'base ''RealName)
makeLenses ''RealName
-- $(deriveJSON defaultOptions ''RealName)

--------------------------------------------------------------------------------

newtype Password = Password { _unPassword :: Text }
                    deriving (Show,Read,Eq,Ord,
                              PathPiece,FromJSON,ToJSON,Typeable)
$(deriveSafeCopy 0 'base ''Password)
makeLenses ''Password



newtype HashedPassword = HashedPassword { _unHashedPassword :: Text }
                    deriving (Show,Read,Eq,Ord,
                              PathPiece,FromJSON,ToJSON,ToMarkup)
$(deriveSafeCopy 0 'base ''HashedPassword)
makeLenses ''HashedPassword

sha1 :: ByteString -> Hash.Digest Hash.SHA1
sha1 = Hash.hash

hash' :: Text -> Text
hash' = T.pack . show . sha1 . B.pack . T.unpack

hashPassword :: Password -> HashedPassword
hashPassword = HashedPassword . hash' . _unPassword

--------------------------------------------------------------------------------

newtype Signature = Signature { _signatureData :: Text }
                    deriving (Show,Read,Eq,Ord,ToJSON,FromJSON,PathPiece,ToMarkup)
$(deriveSafeCopy 0 'base ''Signature)
makeLenses ''Signature

-- instance ToJSON Signature where
--   toJSON = String . T.pack . showSignatureData

-- instance FromJSON Signature where
--   parseJSON (String t) = pure . Signature . B.pack . T.unpack $ t
--   parseJSON _          = mzero

-- instance PathPiece Signature where
--   toPathPiece   = T.pack . B.unpack . _signatureData
--   fromPathPiece = Just . Signature . B.pack . T.unpack

fileSignature    :: MonadIO m => FilePath -> m Signature
fileSignature fp = f <$> hashFile fp
  where
    f :: CryptoAPI.SHA1 -> Signature
    f = Signature . T.pack . show


-- showSignatureData               :: Signature -> String
-- showSignatureData (Signature b) = decodeSignatureData b

-- decodeSignatureData   :: ByteString -> String
-- decodeSignatureData b = case decode b of
--     Left _  -> B.unpack b
--       -- failed to decode, so as a best effort show and treat as Char8
--     Right x -> show (x :: CryptoAPI.SHA1)
