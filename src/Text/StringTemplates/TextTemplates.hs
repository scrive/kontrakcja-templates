-- | module for reading dictionaries of templates from csv files.
-- may be used for reading of csv containing translation templates
module Text.StringTemplates.TextTemplates (getTextTemplates) where

import Data.Char (isSpace, isControl)
import Data.List (isSuffixOf)
import System.IO
import Data.Map (Map)
import qualified Data.Map as M

import Text.ParserCombinators.Parsec (parse)
import Data.CSV (csvFile)

import Text.StringTemplates.Utils

type Schema = [String]

-- | Searches recursively a directory for .csv files and
-- parses them with the following format:
--
-- @
-- \"WHATEVER\", \"column_name1\", \"column_name2\", ...
-- \"name\",       \"value1\",       \"value2\",       ...
-- \"name2\",      \"value3\",       \"value4\",       ...
-- ...
-- @
--
-- and returns a list of (name, value) pairs for the chosen column_name
-- (across all files) (e.g. for \"column_name1\" from example,
-- it would return [(\"name\", \"value1\"), (\"name2\", \"value3\")])
-- if the value is missing for column_name (but it's present in other columns),
-- tries values from other columns (in order)
getTextTemplates :: FilePath -- ^ path of a directory to search for .csv files
                    -> IO (Map String [(String, String)])
getTextTemplates path  = do
  paths <- directoryFilesRecursive path
  M.unionsWith (++) `fmap` mapM getTextTemplatesFromFile paths

-- Parses a .csv file with the following format:
-- (line1): WHATEVER, column_name1, column_name2, ...
-- (lineN): name,     value1,       value2,       ...
-- and returns a map from column name to a list of (name, value) pairs
getTextTemplatesFromFile :: FilePath -> IO (Map String [(String, String)])
getTextTemplatesFromFile path | not $ ".csv" `isSuffixOf` path = return $ M.empty
                              | otherwise = do
  csv <- basicCSVParser path
  case csv of
    [] -> error $ "CSV parsing error in" ++ path ++ "\nEmpty file"
    schemaRow:rows ->
        case schemaRow of
          [] -> error $ "CSV parsing error in" ++ path ++ "\nEmpty schema/first line"
          _:schema -> return $ parseTextTemplates schema rows

-- Takes a schema (list of column names) and a list of rows
-- (row looks like: [name, value1, value2...]
-- and returns a map from column name to a list of (name, value) pairs
parseTextTemplates :: Schema -> [[String]] -> Map String [(String, String)]
parseTextTemplates schema = M.unionsWith (++) . map aux
    where aux :: [String] -> Map String [(String, String)]
          aux [] = error "parseTextTemplates: row cannot be empty"
          aux (name:fields) =
              M.fromList $ zip schema $ map makeList $ filter goodField $
                 zip (repeat name) $ map fixField fields
          fixField = replace '\n' ' '
          goodField (_, f) = any notSpaceOrControl f || (length f >= 6)
          notSpaceOrControl c = not $ isSpace c || isControl c
          replace a b = map $ \x -> if x == a then b else x
          makeList x = [x]

-- Parses csv from filepath (using utf-8)
basicCSVParser :: FilePath -> IO [[String]]
basicCSVParser path =
    withFile path ReadMode $ \h -> do
    hSetEncoding h utf8
    content <- hGetContents h
    case parse csvFile path content of
        Right csv -> return csv
        Left s -> error $ "CSV parse error in " ++ path ++ ": " ++ show s
