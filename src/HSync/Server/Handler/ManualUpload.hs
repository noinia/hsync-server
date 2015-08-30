module HSync.Server.Handler.ManualUpload where

import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.Text as T
import           HSync.Server.Import
import           HSync.Server.Handler.AcidUtils
import           Control.Lens
import           Yesod.Form.Bootstrap3(withPlaceholder)
import           Yesod.Core.Types(FileInfo(..))


--------------------------------------------------------------------------------
-- * Manually Add Dir

-- | Create a new directory based on the info supplied in the form.
-- pre: The path does not already contain a directory or file.
postWebCreateDirR          :: RealmId
                           -> Path -- ^ Directory in which to add a new directory
                           -> Handler Html
postWebCreateDirR ri parent = do
    ((result,_), _) <- runFormPost addDirForm
    case result of
      FormSuccess n    -> let p = parent&pathParts %~ (++ [n]) in
        do
          efv <- createDirectory webClientId ri p NonExistent
                 -- we can only create a directory if it does not exist yet
          case efv of
            Left err -> setMessage $ toHtml err
            Right _  -> setMessage $ "Created directory '" <> toHtml n <> "'."
      FormFailure errs -> let e = mconcat $ map toHtml errs
                          in setMessage $ "Error. Could not add directory " <> e
      FormMissing    -> setMessage "Error. No such form"
    redirect $ ViewRealmR ri parent

-- | Code that generates the widget/form which can be used to create a new directory
webCreateDir           :: RealmId -> Path -> Handler Widget
webCreateDir ri parent = do
    (widget,enctype) <- generateFormPost addDirForm
    return $ $(widgetFile "webPutDir")


-- | The form.
addDirForm :: Form FileName
addDirForm = renderDivs $ areq fileNameField
                               (withPlaceholder "Create new directory" "Directory name")
                               Nothing


fileNameField :: Field Handler FileName
fileNameField = checkMMap (return . f) _unFileName textField
  where
    f t
      | isValidFileName t = Right $ FileName t
      | otherwise         = Left $ ("Invalid filename" :: Text)

-- | Check if a filename is valid
isValidFileName :: Text -> Bool
isValidFileName = T.all isValidChar
  where
                      -- note that in particular / is not a valid char, hence
                      -- we cannot go a directory up
    isValidChar c = C.isAlphaNum c || c `L.elem` ("_ ." :: String)


--------------------------------------------------------------------------------
-- * Putting a file


-- | Upload a file from the webiste. If the file with this path already exist
-- it will be replaced.
postWebStoreFileR           :: RealmId -> Path -> Handler Html
postWebStoreFileR ri parent = do
    ((result,_), _) <- runFormPost addFileForm
    case result of
      FormSuccess fInfo -> do
          let n       = FileName $ fileName fInfo
              p       = parent&pathParts %~ (++ [n])
              fSource = transPipe liftResourceT . fileSourceRaw $ fInfo
          efv <- addFile webClientId ri p NonExistent fSource
                 -- we can only create a File if it does not exist yet
          case efv of
            Left err -> setMessage $ toHtml err
            Right _  -> setMessage $ "Added File '" <> toHtml n <> "'."
      FormFailure errs  -> let e = mconcat $ map toHtml errs
                          in setMessage $ "Error. Could not add file " <> e
      FormMissing    -> setMessage "Error. No such form"
    redirect $ ViewRealmR ri parent

-- | Generate the widget that allows uploading a file
webStoreFile           :: RealmId -> Path -> Handler Widget
webStoreFile ri parent = do
  (widget,enctype) <- generateFormPost addFileForm
  return $ $(widgetFile "webPutFile")


-- | Form to upload a file
addFileForm :: Form FileInfo
addFileForm = renderDivs $ areq fileField "Upload File" Nothing


--------------------------------------------------------------------------------
-- * Deleting a file

getWebDeleteR         :: RealmId -> FileKind -> Path -> Handler Html
getWebDeleteR ri fk p = do
  ev <- deleteFileOrDir webClientId ri p fk
  case ev of
    Left err -> setMessage $ "Error. Could not delete file or directory " <> toHtml p
                           <> " : " <> toHtml err
    Right _  -> setMessage $ "Deleted " <> toHtml p
  redirect $ ViewRealmR ri (parentOf p)
