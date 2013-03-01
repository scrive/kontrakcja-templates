-- |module for reading many template definitions from files
module Text.StringTemplates.Files (getTemplates) where

import Data.Char (isAlphaNum)
import Data.List (intercalate, isPrefixOf)
import Data.Maybe (maybeToList)
import System.IO

-- |parses template definitions read from the file path (using utf8)
-- definitions are separated by >=1 lines starting with '#'
-- if first line of definition looks like foo=bar, then it returns
-- (foo, bar ++ rest of the lines (from definition) concatenated)
-- broken definitons are skipped
--
-- Example template file:
--
-- @
-- foo=
-- \<html>
--   \<body>
--     \<h1> hello \</h1>
--   \</body>
-- \</html>
-- ###
-- bar=\<b>BAR\</b>
-- @
getTemplates :: FilePath -- ^ file path of a file with template definitions
             -> IO [(String, String)]
getTemplates fp =
    withFile fp ReadMode $ \handle -> do
        hSetEncoding handle utf8
        parseTemplates handle

-- parses template definitions from the handle
-- definitions are separated by >=1 lines starting with '#'
-- if first line of definition looks like foo=bar, then it returns
-- (foo, bar ++ rest of the lines (from definition) concatenated)
-- broken definitons are skipped
parseTemplates :: Handle -> IO [(String,String)]
parseTemplates handle = do
    e <- hIsEOF handle
    if (e)
        then return []
        else do
               t  <- parseTemplate handle
               ts <- parseTemplates handle
               return $ (maybeToList t) ++ ts

-- reads many lines from the handle, stopping at
-- first line starting with '#', (read from the handle, not used)
-- if line should looks like "foo=bar", then it returns
-- Just (foo, bar ++ rest of the lines concatenated)
-- otherwise Nothing
parseTemplate :: Handle -> IO (Maybe (String, String))
parseTemplate handle = do
    ls <- parseLines handle
    let (name,t) = break (==  '=') $ head ls
    if (null ls || null (name) || null t)
        then return Nothing
        else do
            let template = intercalate "\r\n" ((tail t): (tail ls))
            return $ Just (filter isAlphaNum name,template)

-- returns list of lines read from the handle
-- stops at first line starting with '#'
-- (it's read from the handle, but not returned)
parseLines :: Handle -> IO [String]
parseLines handle = do
    l <- hGetLine handle
    e <- hIsEOF handle
    if (isPrefixOf ("#") l)
        then return []
        else if e
            then return [l]
            else fmap ((:) l) (parseLines handle)
