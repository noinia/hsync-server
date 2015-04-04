module HSync.Server.Handler.ManualUpload where


import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.Text as T
import           HSync.Server.Handler.FileActions(getFileIdent, putFile, postPutDirR)
import           HSync.Server.Import
import           Yesod.Core.Types(FileInfo(..))


--------------------------------------------------------------------------------
-- * Manually Add Dir

postWebPutDirR           :: Path -- ^ Directory in which to add a new directory
                         -> Handler Html
postWebPutDirR parent = do
    ((result,_), _) <- runFormPost addDirForm
    case result of
      FormSuccess n    -> do
                            let p = parent { subPath = subPath parent ++ [n] }
                            postPutDirR NonExistent p
                            setMessage $ "Created directory '" -- <> n <> "'."
      FormFailure errs -> setMessage "Error. Could not add directory"

                          -- $ "Error. Could not add directory: " : errs
    redirect $ ViewTreeR parent


-- | Code that generates the widget
webPutDir        :: Path -> Handler Widget
webPutDir parent = do
  (widget,enctype) <- generateFormPost addDirForm
  return $ $(widgetFile "webPutDir")


addDirForm :: Form FileName
addDirForm = renderDivs $ areq fileNameField "Directory name" Nothing


-- fileNameField :: RenderMessage (HandlerSite m) FormMessage => Field m FileName
fileNameField = checkBool isValidFileName msg textField
  where
    msg = "Invalid filename" :: Text
    isValidFileName = not . T.any isInvalidChar

                      -- note that in particular / is not a valid char, hence
                      -- we cannot go a directory up
    isInvalidChar c = C.isAlphaNum c || c `L.elem` "_ ."



--------------------------------------------------------------------------------
-- * Putting a file

postWebPutFileR        :: Path -> Handler Html
postWebPutFileR parent = do
    ((result,_), _) <- runFormPost addFileForm
    case result of
      FormSuccess fInfo -> do
                             let p = parent { subPath = subPath parent ++ [fileName fInfo] }
                             fi   <- getFileIdent p
                             fp   <- asLocalPath p
                             eNot <- putFile fi p (fileSourceRaw fInfo) fp
                             case eNot of
                               Left err -> setMessage $ "Error saving file:"
                               Right _  -> setMessage "File saved."
                             setMessage "File st"
      FormFailure errs -> setMessage  "Error. Could not add file"
    redirect $ ViewTreeR parent


webPutFile        :: Path -> Handler Widget
webPutFile parent = do
  (widget,enctype) <- generateFormPost addFileForm
  return $ $(widgetFile "webPutFile")


addFileForm :: Form FileInfo
addFileForm = renderDivs $ areq fileField "Upload File" Nothing
