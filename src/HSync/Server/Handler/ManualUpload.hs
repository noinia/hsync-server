module HSync.Server.Handler.ManualUpload where

import Data.Monoid(mappend)
import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.Text as T
import           HSync.Server.Import
import           Control.Lens
import           Yesod.Form.Bootstrap3(withPlaceholder)

--------------------------------------------------------------------------------
-- * Manually Add Dir



createDirectory                     :: RealmId -> Path -> FileKind
                                    -> Handler (Either ErrorMessage FileVersion)
createDirectory ri p currentKind = do
    dt  <- currentTime
    mui <- (fmap (^.userId)) <$> maybeAuthId
    case mui of
      Just ui -> updateAcid $ AddDirectory ri p currentKind (LastModified dt ui webClientId)
      Nothing -> return $ Left "Error. Create Directory needs a userId"


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
          efv <- createDirectory ri p NonExistent
                 -- we can only create a directory if it does not exist yet
          case efv of
            Left err -> setMessage $ toHtml err
            Right _  -> setMessage $ "Created directory '" <> toHtml n <> "'."
      FormFailure errs -> let e = mconcat $ map toHtml errs
                          in setMessage $ "Error. Could not add directory " <> e
    redirect $ ViewTreeR ri parent

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
postWebStoreFileR ri parent = error "storefile"

  -- do
  --   ((result,_), _) <- runFormPost addFileForm
  --   case result of
  --     FormSuccess fInfo -> do
  --                            let p  = Path u (sp ++ [fileName fInfo])
  --                                ci = webClientIdent
  --                            fi   <- getFileIdent p
  --                            t <- putFile ci fi p (fileSourceRaw fInfo)
  --                            setMessage $ toHtml t
  --     FormFailure errs -> let e = mconcat $ map toHtml errs
  --                         in setMessage $  "Error. Could not add file: " <> e
  --   redirect $ ViewTreeR parent

-- | Generate the widget that allows uploading a file
webStoreFile           :: RealmId -> Path -> Handler Widget
webStoreFile ri parent = do
  (widget,enctype) <- generateFormPost addFileForm
  return $ $(widgetFile "webPutFile")


-- | Form to upload a file
addFileForm :: Form FileInfo
addFileForm = renderDivs $ areq fileField "Upload File" Nothing
