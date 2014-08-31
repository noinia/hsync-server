module HSync.Server.Handler.Auth where

import Control.Applicative
import Control.Monad(when)

import Data.Maybe

import HSync.Common.Header(lookupTypedHeader, HUserIdent(..), HPassword(..))

import HSync.Server.Import
import HSync.Server.AcidSync
import HSync.Server.AcidState

import HSync.Server.LocalAuth(validateUser)

import HSync.Server.FileSystemState(newUserFSState)

import HSync.Server.User(User(..),RealName(..))

import System.Directory(createDirectory)



import qualified Data.Text as T

--------------------------------------------------------------------------------

instance ToContent Bool where
  toContent = toContent . show

instance ToTypedContent Bool where
  toTypedContent = TypedContent typePlain . toContent


postMyLoginR :: Handler Bool
postMyLoginR = do
    mu@(Just u) <-                         lookupTypedHeader HUserIdent
    mpw         <- fmap hashedPassword <$> lookupTypedHeader HPassword
    b <- validateUser' mu mpw
    when b $ setCreds False $ Creds "PostLoginR" (unUI u) []
    return b
  where
    validateUser' mu mpw = fromMaybe (pure False) $ liftA2 validateUser mu mpw

--------------------------------------------------------------------------------
-- | permissions

requireRead             :: Path -> Handler Bool
requireRead (Path ui _) = (\u -> ui == userId u) <$> requireAuthId'

-- | For now require the same thing as reading
requireWrite :: Path -> Handler Bool
requireWrite = requireRead


requireAuthId' :: Handler User
requireAuthId' = maybeAuthId >>= maybe (permissionDenied "Login Required") return
