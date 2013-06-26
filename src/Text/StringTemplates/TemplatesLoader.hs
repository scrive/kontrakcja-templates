{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | Module for reading templates from files
module Text.StringTemplates.TemplatesLoader ( Templates
                                            , GlobalTemplates
                                            , localizedVersion
                                            , readGlobalTemplates
                                            , renderTemplateMain
                                            , getTemplatesModTime
                                            ) where

import Data.List (isSuffixOf)
import Text.StringTemplate
import Text.StringTemplate.Classes
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Map as Map
import Text.Html (stringToHtmlString)
import Data.Time.Clock
import Text.StringTemplates.Files
import Text.StringTemplates.TextTemplates
import Text.StringTemplates.Utils

-- | Group of string templates
type Templates = STGroup String
-- | Global map of templates (for a project),
--   indexed by a column name (for text string templates, see TextTemplates for doc)
type GlobalTemplates = Map.Map String Templates

-- | Retrieve templates for specified column name
localizedVersion :: String -> GlobalTemplates -> Templates
localizedVersion col mtemplates = mtemplates Map.! col

-- Fixme: Make this do only one read of all files !!
-- | Reads text templates and templates from files (see TextTemplates and Files modules docs respectively).
--   List of text columns is used to load text templates for every column (the rest of them are
--   used as fallback columns)
readGlobalTemplates :: MonadIO m =>
                      FilePath   -- ^ dir path to recursively scan for .csv files containing text templates
                    -> FilePath   -- ^ dir path to recursively scan for .st files containing string templates
                    -> m GlobalTemplates
readGlobalTemplates textTemplatesFilePath templatesDirPath  = do
  files <- liftIO $ directoryFilesRecursive templatesDirPath
  let templatesFilePaths = filter (".st" `isSuffixOf`) files
  ts <- liftIO $ mapM getTemplates templatesFilePaths
  tts <- liftIO $ getTextTemplates textTemplatesFilePath
  liftM Map.fromList $ forM (Map.keys tts) $ \col -> do
    checked <- mapM newCheckedTemplate $ (concat ts) ++ (tts Map.! col)
    return ((col, groupStringTemplates checked)::(String, Templates))

newCheckedTemplate :: Monad m => (String, String) -> m (String, StringTemplate String)
newCheckedTemplate (n,v) = do
  let t = newSTMP v
      (errors, _, _) = checkTemplate t
  maybe (return ()) (\e -> fail $ "newCheckedTemplate: problem with template " ++ show n ++ ": " ++ e) errors
  return (n,t)

-- | Returns the latest modification time across all template files
getTemplatesModTime :: FilePath   -- ^ path to dir containing .csv files with template files
                    -> FilePath -- ^ dir path to recursively scan for .st files containing string templates
                    -> IO UTCTime
getTemplatesModTime textTemplatesDir templatesDirPath = do
    mt1 <- getRecursiveMTime templatesDirPath
    mt2 <- getRecursiveMTime textTemplatesDir
    return $ maximum $ [mt1,mt2]

-- | main template rendering function.
--   renders template by name (it's an error to render template that's not present in templates group),
--   and using list of named template params. simple 'noescape' template is added for convenience
renderTemplateMain :: ToSElem a =>
                     Templates     -- ^ group of templates
                   -> String        -- ^ template name
                   -> [(String, a)] -- ^ named template params
                   -> (StringTemplate String -> StringTemplate String) -- ^ additional template altering function
                   -> String -- ^ rendered template
renderTemplateMain ts name params f = case mt of
  Just t  -> render $ f (setManyAttrib params t)
  Nothing -> error  $ "No template named " ++ name
  where
    ts' = setEncoderGroup stringToHtmlString ts
    noescape = groupStringTemplates [("noescape", newSTMP "$it$" :: StringTemplate String)]
    mt = getStringTemplate name $ mergeSTGroups noescape ts'

{- For some reasons the SElem a is not of class ToSElem -}
instance (Stringable a) => ToSElem (SElem a) where
  toSElem (STR a) = (STR a)
  toSElem (BS a) = (BS a)
  toSElem (STSH a) = (STSH a)
  toSElem (SM a) = (SM $ fmap (toSElem) a)
  toSElem (LI a) = (LI $ fmap (toSElem) a)
  toSElem (SBLE a) = (SBLE $ convert a)
  toSElem (SNAT a) = (SNAT $ convert a)
  toSElem (TXT a) = (STR $ convert a)
  toSElem SNull = SNull

convert :: (Stringable a, Stringable b) => a -> b
convert = stFromString . stToString
