
module Manifesto.FileList
    ( FileList(..)
    , mkFileList
    , mkPathProducer
    ) where

import Control.Monad
import Control.Monad.Catch (MonadThrow)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time
import qualified Data.Vector as V
import Path
import Pipes hiding (for, each)
import System.IO (hFlush, stdout)
import System.Process

import Manifesto.Types

data FileList = FileList
    { _flHeader :: !Header
    , _flFiles  :: !(V.Vector (Path Rel File))
      -- ^ NOTE: This use of Vector probably doesn't buy us much over List,
      -- and the conversion may not be worth it. Benchmark.
    }

-- | Create a Producer which yields the paths in the given FileList.
mkPathProducer
    :: (Monad m, Monoid a) => FileList -> Producer (Maybe (Path Rel File)) m a
mkPathProducer (FileList _ files) = do
    foldr (\a p -> yield (Just a) >> p) (yield Nothing >> return mempty) files

mkFileList :: Path Abs Dir -> Maybe (Path Abs File) -> IO FileList
mkFileList root excludeFile = do
    putStr "Generating file list..."; hFlush stdout
    header <- mkHeader root excludeFile
    paths <- mkPaths header
    putStrLn $ " done (" ++ show (V.length paths) ++ " files)"
    return (FileList header paths)

mkHeader :: Path Abs Dir -> Maybe (Path Abs File) -> IO Header
mkHeader root excludeFile = do
    host <- getHostname
    ts   <- getCurrentTime
    excludePatterns <- case excludeFile of
        Nothing -> return []
        Just p  -> T.lines <$> T.readFile (toFilePath p)
    return $ Header
        { _hHost            = host
        , _hRoot            = root
        , _hExcludePatterns = excludePatterns
        , _hTimestamp       = ts
        }

-- | TODO: better exclude patterns
mkPaths :: Header -> IO (V.Vector (Path Rel File))
mkPaths header = do
    filePaths <- V.fromList <$> listDirectoryRecursive (_hRoot header)
    return $ V.filter (not . isExcluded) filePaths
  where
    isExcluded path = any
        (\x -> not (T.null x) && x `T.isPrefixOf` T.pack (toFilePath path))
        (_hExcludePatterns header)

listDirectoryRecursive
    :: (MonadIO m,MonadThrow m)
    => Path Abs Dir -> m [Path Rel File]
listDirectoryRecursive root = do
    output <- liftIO $ readProcess "find" [toFilePath root, "-type", "f"] ""
    mapM (stripDir root <=< parseAbsFile) (lines output)

{-

listDirectoryRecursive'
    :: (MonadIO m,MonadThrow m)
    => Path Abs Dir -> m [Path Rel File]
listDirectoryRecursive' root = do
    (subdirs, files) <- listDirectory root
    subfiles <- mapM handleSub subdirs
    return $ files ++ concat subfiles
  where
    handleSub dir = map (dir </>) <$> listDirectoryRecursive (root </> dir)

-- | List objects in a directory, excluding "." and "..".  Entries are not
-- sorted. Stolen from stack.Path.IO.
-- TODO: optimise.
listDirectory
    :: (MonadIO m,MonadThrow m)
    => Path Abs Dir -> m ([Path Rel Dir], [Path Rel File])
listDirectory dir = do
    let dirFP = toFilePath dir
    entriesFP <- liftIO (getDirectoryContents dirFP)
    maybeEntries <-
      forM (map (dirFP ++) entriesFP)
           (\entryFP ->
              do isDir <- liftIO (doesDirectoryExist entryFP)
                 if isDir
                    then case parseAbsDir entryFP of
                           Nothing -> return Nothing
                           Just entryDir ->
                             if dir `isParentOf` entryDir
                                then return (Just (Left entryDir))
                                else return Nothing
                    else case parseAbsFile entryFP of
                           Nothing -> return Nothing
                           Just entryFile -> return (Just (Right entryFile)))
    let absEntries = catMaybes maybeEntries
    let relDirs  = mapMaybe (stripDir dir) $ lefts  absEntries
    let relFiles = mapMaybe (stripDir dir) $ rights absEntries
    return (relDirs, relFiles)

-}

