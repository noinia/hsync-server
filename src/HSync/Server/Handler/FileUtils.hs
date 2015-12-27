module HSync.Server.Handler.FileUtils where

import Data.Monoid(Last(..))
import HSync.Server.Import.NoFoundation
import HSync.Server.Foundation
import Control.Lens
import System.Directory(createDirectoryIfMissing, renameFile)
import System.FilePath(takeExtension)
import qualified System.FilePath as FP
import qualified Data.Text as T
import System.Random(randomRIO)
import qualified Data.Map as M

--------------------------------------------------------------------------------

storeFile        :: RealmId -> Path -> Source Handler ByteString
                 -> Handler (FilePath, Signature)
storeFile ri p s = do
    dirFP <- toFilePath ri p
    rnd <- liftIO randomFileName
    liftIO $ createDirectoryIfMissing True dirFP
    let fp = dirFP </> rnd
    s $$ sinkFile fp
    sig <- fileSignature fp
    let fp' = dirFP </> T.unpack (sig^.signatureData)
    liftIO $ renameFile fp fp'
    return (fp',sig)


randomFileName :: IO FilePath
randomFileName = replicateM randomFNameLength (randomRIO ('a','z'))
  where
    randomFNameLength = 20


toFilePath                      :: RealmId -> Path -> Handler FilePath
toFilePath (RealmId i) (Path p) = f <$> getYesod
  where
    p'  = map (\n -> T.unpack $ n^.unFileName) p
    f y = FP.joinPath $ [ y^.appSettings.filesPath
                        , show i
                        ] ++ p'


-- | Get the path where a file is stored
getFilePath        :: RealmId -> Path -> Signature -> Handler FilePath
getFilePath ri p s = (</> T.unpack (s^.signatureData)) <$> toFilePath ri p


fileExtension :: Path -> Maybe Text
fileExtension = fmap f . last' . (^.pathParts)
  where
    f       = T.pack . takeExtension . T.unpack . T.toLower . (^.unFileName)
    last'   = getLast . foldMap (Last . Just)


contentTypeOf   :: Path -> ContentType
contentTypeOf p = fromMaybe typeOctet (flip M.lookup typeByExt =<< fileExtension p)


typeByExt :: M.Map Text ContentType
typeByExt = M.fromList [ ("html",typeHtml)
                       , ("txt",typePlain)
                       , ("json",typeJson)
                       , ("xml",typeXml)
                       , ("atom",typeAtom)
                       , ("rss",typeRss)
                       , ("jpg",typeJpeg)
                       , ("jpeg",typeJpeg)
                       , ("png",typePng)
                       , ("gif",typeGif)
                       , ("svg",typeSvg)
                       , ("js",typeJavascript)
                       , ("css",typeCss)
                       , ("flv",typeFlv)
                       , ("ogv",typeOgv)
                       ]
