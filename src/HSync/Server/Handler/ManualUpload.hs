module HSync.Server.Handler.ManualUpload where


import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.Text as T

import           HSync.Common.Types
import           HSync.Server.Import
import           Yesod.Core.Types(FileInfo(..))
import           Yesod.Form.Bootstrap3(withPlaceholder)

--------------------------------------------------------------------------------
-- * Manually Add Dir


-- | ClientIdent to use if we are uploading something from the website.
webClientIdent :: ClientName
webClientIdent = ClientName "web"


-- | Create a new directory based on the info supplied in the form.
postWebCreateDirR        :: Path -- ^ Directory in which to add a new directory
                         -> Handler Html
postWebCreateDirR parent = undefined


  -- do
  --   ((result,_), _) <- runFormPost addDirForm
  --   case result of
  --     FormSuccess n    -> do
  --                           let p = parent { subPath = subPath parent ++ [n] }
  --                           _ <- postPutDirR' NonExistent p webClientIdent
  --                           setMessage $ "Created directory '" <> toHtml n <> "'."
  --     FormFailure errs -> let e = mconcat $ map toHtml errs
  --                         in setMessage $ "Error. Could not add directory " <> e
  --   redirect $ ViewTreeR parent

-- | Code that generates the widget/form which can be used to create a new directory
webCreateDir        :: Path -> Handler Widget
webCreateDir parent = do
  (widget,enctype) <- generateFormPost addDirForm
  return $ $(widgetFile "webPutDir")

-- | The form.
addDirForm :: Form FileName
addDirForm = renderDivs $ areq fileNameField
                               (withPlaceholder "Create new directory" "Directory name")
                               Nothing


fileNameField :: Field Handler FileName
fileNameField = undefined
--  checkBool isValidFileName msg textField
  where
    msg = "Invalid filename" :: Text

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
postWebStoreFileR        :: Path -> Handler Html
postWebStoreFileR parent = undefined

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
webStoreFile        :: Path -> Handler Widget
webStoreFile parent = do
  (widget,enctype) <- generateFormPost addFileForm
  return $ $(widgetFile "webPutFile")


-- | Form to upload a file
addFileForm :: Form FileInfo
addFileForm = renderDivs $ areq fileField "Upload File" Nothing
