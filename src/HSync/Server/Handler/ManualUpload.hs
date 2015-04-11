module HSync.Server.Handler.ManualUpload where


import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.Text as T
import           HSync.Server.Handler.FileActions(getFileIdent, putFile, postPutDirR')
import           HSync.Server.Import
import           Yesod.Core.Types(FileInfo(..))
import           Yesod.Form.Bootstrap3(withPlaceholder)

--------------------------------------------------------------------------------
-- * Manually Add Dir


-- | ClientIdent to use if we are uploading something from the website.
webClientIdent :: ClientIdent
webClientIdent = ClientIdent "web"


-- | Create a new directory based on the info supplied in the form.
postWebPutDirR           :: Path -- ^ Directory in which to add a new directory
                         -> Handler Html
postWebPutDirR parent = do
    ((result,_), _) <- runFormPost addDirForm
    case result of
      FormSuccess n    -> do
                            let p = parent { subPath = subPath parent ++ [n] }
                            _ <- postPutDirR' NonExistent p webClientIdent
                            setMessage $ "Created directory '" <> toHtml n <> "'."
      FormFailure errs -> let e = mconcat $ map toHtml errs
                          in setMessage $ "Error. Could not add directory " <> e
    redirect $ ViewTreeR parent

-- | Code that generates the widget/form which can be used to create a new directory
webPutDir        :: Path -> Handler Widget
webPutDir parent = do
  (widget,enctype) <- generateFormPost addDirForm
  return $ $(widgetFile "webPutDir")

-- | The form.
addDirForm :: Form FileName
addDirForm = renderDivs $ areq fileNameField
                               (withPlaceholder "Create new directory" "Directory name")
                               Nothing


fileNameField :: Field Handler FileName
fileNameField = checkBool isValidFileName msg textField
  where
    msg = "Invalid filename" :: Text

-- | Check if a filename is valid
isValidFileName = T.all isValidChar
  where
                      -- note that in particular / is not a valid char, hence
                      -- we cannot go a directory up
    isValidChar c = C.isAlphaNum c || c `L.elem` "_ ."


--------------------------------------------------------------------------------
-- * Putting a file


-- | Upload a file from the webiste.
postWebPutFileR        :: Path -> Handler Html
postWebPutFileR parent@(Path u sp) = do
    ((result,_), _) <- runFormPost addFileForm
    case result of
      FormSuccess fInfo -> do
                             let p  = Path u (sp ++ [fileName fInfo])
                                 ci = webClientIdent
                             fi   <- getFileIdent p
                             fp   <- asLocalPath p
                             eNot <- putFile ci fi p (fileSourceRaw fInfo) fp
                             case eNot of
                               Left err -> setMessage $
                                             "Error saving file:" <> toHtml (show err)
                               Right _  -> setMessage "File saved."
                             setMessage "File st"
      FormFailure errs -> let e = mconcat $ map toHtml errs
                          in setMessage $  "Error. Could not add file: " <> e
    redirect $ ViewTreeR parent

-- | Generate the widget that allows uploading a file
webPutFile        :: Path -> Handler Widget
webPutFile parent = do
  (widget,enctype) <- generateFormPost addFileForm
  return $ $(widgetFile "webPutFile")


-- | Form to upload a file
addFileForm :: Form FileInfo
addFileForm = renderDivs $ areq fileField "Upload File" Nothing
