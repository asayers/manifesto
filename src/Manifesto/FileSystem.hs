{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module Manifesto.FileSystem
    ( regularDescendentsOf
    , readFileLines
    , writeFileLines
    , writeLinesWithTmpFile
    , getWorkingDir
    ) where

import Control.Exception (onException)
import Control.Monad
import qualified Data.List as L
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T
import Data.Time.Clock.POSIX
import Path
import Pipes
import Pipes.Safe hiding (onException)
import qualified Pipes.Prelude as P
import qualified Pipes.Safe.Prelude as PS
import qualified Pipes.ByteString as PB
import System.Directory
import System.Posix (openDirStream, readDirStream, closeDirStream)
import System.Posix.Files
import qualified System.IO as IO

import Manifesto.Types

-------------------------------------------------------------------------------
-- Analysing files

data FSNode
    = FSRegularFile     (Path Abs File) LastModified
    | FSDirectory       (Path Abs Dir )
    | FSSymbolicLink    (Path Abs File)
    | FSSocket          (Path Abs File)
    | FSNamedPipe       (Path Abs File)
    | FSBlockDevice     (Path Abs File)
    | FSCharacterDevice (Path Abs File)
    deriving (Show)

getFSNode :: FilePath -> IO (Maybe FSNode)
getFSNode path = do
    exists <- fileExist path'
    if exists
      then parsePath <$> getSymbolicLinkStatus path'
      else return Nothing
  where
    path' = L.dropWhileEnd (== '/') path
    parsePath fs
        | isRegularFile     fs = FSRegularFile     <$> parseAbsFile path' <*> pure (posixSecondsToUTCTime $ modificationTimeHiRes fs)
        | isDirectory       fs = FSDirectory       <$> parseAbsDir  path'
        | isSymbolicLink    fs = FSSymbolicLink    <$> parseAbsFile path'
        | isSocket          fs = FSSocket          <$> parseAbsFile path'
        | isNamedPipe       fs = FSNamedPipe       <$> parseAbsFile path'
        | isBlockDevice     fs = FSBlockDevice     <$> parseAbsFile path'
        | isCharacterDevice fs = FSCharacterDevice <$> parseAbsFile path'
        | otherwise = Nothing

-------------------------------------------------------------------------------
-- Traversing the directory hierarchy

-- | Returns the filesystem entries which are direct children of the given
-- directory. Returns no entries if the given directory doesn't exist, or
-- is unreadable.
--
-- Note that this function gives no guarantees about the ordering of the
-- results.
childOf
    :: MonadSafe m
    => Path Abs Dir    -- ^ Path to the root directory
    -> ListT m FilePath
childOf rootPath = Select $ do
    canRead <- liftIO $ (readable <$> getPermissions (toFilePath rootPath))
                  `onException` (return False)
    when canRead $ bracket
        (liftIO $ openDirStream (toFilePath rootPath))
        (liftIO . closeDirStream)
        go
  where
    go dirPath = do
        relEntryPath <- liftIO $ readDirStream dirPath
        case relEntryPath of
            ""   -> return ()
            "."  -> go dirPath
            ".." -> go dirPath
            _    -> do
                let absEntryPath = toFilePath rootPath ++ relEntryPath
                yield absEntryPath
                go dirPath
{-# INLINABLE childOf #-}

-- | This funciton does a recursive descent, collecting files at each level
-- so that it can order them. Note that this means that at any point, it
-- retains the lists of occupants of the current directory and all parent
-- directories. Therefore it's very much *not* a constant-memory Producer.
regularDescendentsOf
    :: MonadSafe m
    => Path Abs Dir
    -> Exclusions
    -> Producer (Path Rel File, LastModified) m ()
regularDescendentsOf rootPath exclusions =
    go rootPath
  where
    go path = do
        children <- L.sort <$> P.toListM (every (childOf path))
        forM_ children $ \fp -> do
            let skip = liftIO $ putStrLn ("ignoring: " ++ fp)
            fsNode <- liftIO $ getFSNode fp
            case fsNode of
                _ | exclusions `exclude` fp  -> skip
                Just (FSDirectory       p   ) -> go p
                Just (FSRegularFile     p ts) -> do
                    relPath <- stripDir rootPath p
                    yield (relPath, ts)
                Just (FSSymbolicLink    _   ) -> skip
                Just (FSSocket          _   ) -> skip
                Just (FSNamedPipe       _   ) -> skip
                Just (FSBlockDevice     _   ) -> skip
                Just (FSCharacterDevice _   ) -> skip
                Nothing                       -> skip

exclude :: Exclusions -> FilePath -> Bool
exclude exclusions fp =
    any (T.pack fp `T.isPrefixOf`) exclusions

-------------------------------------------------------------------------------
-- Reading and writing files

-- Note: only use if you're happy for lines to fit in memory
readFileLines
    :: MonadSafe m => Path Abs File -> Producer T.Text m ()
readFileLines path = do
    exists <- liftIO $ doesFileExist (toFilePath path)
    if exists
      then PS.withFile (toFilePath path) IO.ReadMode go
      else return ()
  where
    go h = flip catchIOError (const $ return ()) $ do
      txt <- liftIO (T.hGetLine h)
      yield txt
      go h

writeFileLines
    :: MonadSafe m => Path Abs File -> Consumer T.Text m ()
writeFileLines path =
    PS.withFile (toFilePath path) IO.WriteMode hWriteLinesUtf8

hWriteLinesUtf8
    :: MonadIO m => IO.Handle -> Consumer T.Text m ()
hWriteLinesUtf8 h =
    P.map (T.encodeUtf8 . (<> "\n")) >-> PB.toHandle h

-------------------------------------------------------------------------------
-- Working with temporary files

-- Open a temporary file in the system temp directory and write lines to it.
-- When the pipeline terminates, or something goes wrong, copy the
-- temporary file to the given path.
writeLinesWithTmpFile
    :: MonadSafe m => Path Abs File -> Consumer T.Text m ()
writeLinesWithTmpFile path = do
    tmpPath <- liftIO getTmpPath
    bracket (acquireH tmpPath) (releaseH tmpPath) hWriteLinesUtf8
  where
    acquireH tmpPath = liftIO $ do
        IO.openFile tmpPath IO.WriteMode
    releaseH tmpPath h = liftIO $ do
        IO.hClose h
        copyFile tmpPath (toFilePath path)
    getTmpPath = do
        dir <- getTemporaryDirectory
        ts  <- getPOSIXTime
        return $ dir ++ "/" ++ toFilePath (filename path) ++ show ts

-- Open a temporary file in the system temp directory using the given path
-- as a template and perform the given action. If something goes wrong,
-- copy the contents of the file path to the given path, suffixed with
-- a tilde.
-- withSystemTempFile' :: Path Abs File -> (FilePath -> IO.Handle -> IO a) -> IO a
-- withSystemTempFile' path fn =
--     IO.withSystemTempFile (toFilePath $ filename path) $ \pTmp hTmp ->
--         fn pTmp hTmp `onException` do
--             IO.hClose hTmp
--             copyFile pTmp (toFilePath path <> "~")

-------------------------------------------------------------------------------
-- Other utilities

getWorkingDir :: (MonadIO m) => m (Path Abs Dir)
getWorkingDir = liftIO (canonicalizePath "." >>= parseAbsDir)
