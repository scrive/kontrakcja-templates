{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | Module for reading templates from files
module Text.StringTemplates.TemplatesLoader ( Templates
                                            , GlobalTemplates
                                            , localizedVersion
                                            , readGlobalTemplates
                                            , renderTemplateMain
                                            , getTemplatesModTime
                                            ) where

import Data.List (isSuffixOf,find)
import Data.Maybe (fromMaybe)
import Text.StringTemplate
import Text.StringTemplate.Classes
import Control.Monad
import qualified Control.Monad.Fail as MF
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
--   indexed by a language name (for text string templates, see TextTemplates for doc)
type GlobalTemplates = Map.Map String Templates

-- | Retrieve templates for specified language name
localizedVersion :: String -> GlobalTemplates -> Templates
localizedVersion col mtemplates = fromMaybe (error $ "localizedVersion: undefined language: " ++ show col) $ Map.lookup col mtemplates

-- Fixme: Make this do only one read of all files !!
-- | Reads text templates and templates from files (see TextTemplates and Files modules docs respectively).
readGlobalTemplates :: (MF.MonadFail m, MonadIO m) =>
                      FilePath   -- ^ dir path to recursively scan for .json files containing text templates
                    -> FilePath  -- ^ dir path to recursively scan for .st files containing string templates
                    -> String    -- ^ default language. We can guarantee that empty language texts will be replaced
                    -> m GlobalTemplates
readGlobalTemplates textTemplatesFilePath templatesDirPath  defaultLang = do
  files <- liftIO $ directoryFilesRecursive templatesDirPath
  let templatesFilePaths = filter (".st" `isSuffixOf`) files
  ts <- liftIO $ mapM getTemplates templatesFilePaths
  tts <- liftIO $ getTextTemplates textTemplatesFilePath
  let defaultLangTemplates = case (find (\l -> defaultLang == fst l) $ Map.toList tts) of
                               Just (_,t) -> t
                               Nothing -> error $ "Default language " ++ defaultLang ++ " is not defined."
  liftM Map.fromList $ forM (Map.toList tts) $ \(col,t) -> do
    let  t' =  fixTT t defaultLangTemplates
    checked <- mapM newCheckedTemplate $ (concat ts) ++ t'
    return ((col, groupStringTemplates checked)::(String, Templates))

-- All empty templates will be replaced by values from default lang.
-- Missing templates from defaul lang will be added
fixTT:: [(String, String)] -> [(String, String)] -> [(String, String)]
fixTT [] d = d
fixTT ((n,""):r) d = case find (\x -> n == fst x) d of
                          Just t -> t :  fixTT r (filter (\x -> n /= fst x) d)
                          Nothing -> (n,"") : fixTT r d
fixTT ((n,v):r) d = (n,v) :  fixTT r (filter (\x -> n /= fst x) d)



newCheckedTemplate :: (MF.MonadFail m, Monad m) => (String, String) -> m (String, StringTemplate String)
newCheckedTemplate (n,v) = do
  let t = newSTMP v
      (errors, _, _) = checkTemplate t
  maybe (return ()) (\e -> MF.fail $ "newCheckedTemplate: problem with template " ++ show n ++ ": " ++ e) errors
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
