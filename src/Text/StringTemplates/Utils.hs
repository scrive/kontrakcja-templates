{-
  Util modul. Shold be dropped when misc is released as package.
-}

module Text.StringTemplates.Utils (directoryEntriesRecursive,directoryFilesRecursive,getRecursiveMTime) where

import System.Directory
import Data.List (isSuffixOf)
import Data.Time.Clock (UTCTime)

directoryEntriesRecursive :: FilePath -- ^ dir path to be searched for recursively
                          -> IO ([FilePath], [FilePath]) -- ^ (list of all subdirs, list of all files)
directoryEntriesRecursive path | "." `isSuffixOf` path = return ([], [])
                               | otherwise = do
  isDir <- doesDirectoryExist path
  if isDir then do
      entries <- getDirectoryContents path
      let properEntries = map ((path ++ "/")++) entries
      results <- mapM directoryEntriesRecursive properEntries
      let (dirs, files) = biConcat results
      return (path:dirs, files)
   else
      return ([], [path])
 where biConcat = (\(x, y) -> (concat x, concat y)) . unzip

directoryFilesRecursive :: FilePath -- ^ dir path to be searched for recursively
                        -> IO [FilePath] -- ^ list of all files in that dir
directoryFilesRecursive path = snd `fmap` directoryEntriesRecursive path

-- | Check recursively time of modification of any file(or dir) in directory
getRecursiveMTime :: FilePath -> IO UTCTime
getRecursiveMTime path = do
  (dirs, files) <- directoryEntriesRecursive path
  mtimes <- mapM getModificationTime $ dirs ++ files
  return $ maximum mtimes
