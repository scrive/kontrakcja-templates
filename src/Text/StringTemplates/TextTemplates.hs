-- | module for reading dictionaries of templates from csv files.
-- may be used for reading of csv containing translation templates
module Text.StringTemplates.TextTemplates (getTextTemplates) where

import qualified Data.Map as M
import Text.JSON
import System.Directory
import Data.List
import Data.Maybe
import Control.Monad

--- | Searches recursively give directory for translations
---   path/lang_code/*.json will match.
getTextTemplates :: FilePath -- ^ path of a directory to search for .json files
                    -> IO (M.Map String [(String, String)])
getTextTemplates path  = do
  dirs <- getDirectoryContents path
  list <- forM dirs $ \d -> do
    isDir <- doesDirectoryExist $ path ++ "/" ++ d
    if (not isDir || "." `isSuffixOf` d)
       then return $ Nothing
       else do
         entries <- filter (\s -> ".json" `isSuffixOf` s) <$> getDirectoryContents (path ++ "/" ++ d)
         translations <- forM entries (\e -> readTranslationFile $ path ++ "/" ++ d ++ "/" ++ e)
         return $ Just (d,concat translations)
  return $ M.fromList $ map fromJust $ filter isJust list


readTranslationFile  :: String -> IO [(String,String)]
readTranslationFile file = do
  mjson <- readFile $ file
  case decode mjson of
     Ok js -> return $ sort $ textsFromJSON $ js
     e -> error $ "Can't parse json with message " ++ show e ++ "  for json" ++ mjson


textsFromJSON :: JSValue -> [(String,String)]
textsFromJSON (JSObject jso) = map (\(a,JSString js) -> (a, fromJSString js)) (fromJSObject jso)
textsFromJSON _ = error "While decoding JSON with translations"
