{-# LANGUAGE OverloadedStrings #-}

import Control.Exception
import Control.Lens
import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State
import Data.Maybe
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Path
import Pipes hiding (for, each)
import qualified Pipes.Prelude as P
import qualified Pipes.Text.IO as PT
import System.Directory
import System.Environment
import System.IO (hClose, Handle)
import System.IO.Temp (withSystemTempFile)

import Manifesto.FileList
import Manifesto.Serialize
import Manifesto.Types


-------------------------------------------------------------------------------
-- Helpers for file IO

readManifest :: Path Abs File -> IO (Maybe Manifest)
readManifest manifestFile = runMaybeT $ do
    let path = toFilePath manifestFile
    guard =<< (lift $ doesFileExist path :: MaybeT IO Bool)
    manifest <- MaybeT $ preview _Manifest <$> T.readFile path
    return manifest

writeManifest :: Path Abs File -> Manifest -> IO ()
writeManifest manifestFile manifest =
    let path = toFilePath $ manifestFile
    in T.writeFile path (review _Manifest manifest)

getWorkingDir :: (MonadIO m) => m (Path Abs Dir)
getWorkingDir = liftIO (canonicalizePath "." >>= parseAbsDir)

-------------------------------------------------------------------------------
-- Logic for making manifests

-- TODO: allow existing manifests with different roots; handle properly
mkManifest
    :: Bool         -- ^ Print verbose output?
    -> [Manifest]   -- ^ Existing manifests to use
    -> FileList     -- ^ The files to include
    -> Producer Entry IO Stats
mkManifest verbose oldManifests fileList = do
    -- forM_ oldManifests $ sanityCheck' fileList
    mkPathProducer fileList >-> mkEntries verbose oldManifests (_flHeader fileList)

mkEntries
    :: Bool -> [Manifest] -> Header
    -> Pipe (Maybe (Path Rel File)) Entry IO Stats
mkEntries verbose oldManifests header =
    go mempty
  where
    lookupExisting path modTime = listToMaybe $ do
        manifest <- oldManifests
        guard $ (_hTimestamp $ _mHeader manifest) > modTime
        maybeToList $ lookupHash path manifest

    go stats = do
        mbPath <- await
        case mbPath of
            Nothing -> return stats
            Just path -> do
                (hsh, stats') <- lift $ runStateT (foo path) stats
                yield $ Entry path hsh
                go stats'

    foo :: Path Rel File -> StateT Stats IO SHA1
    foo path = do
        modTime <- lift $ getModificationTime (toFilePath path)
        case lookupExisting path modTime of
            Nothing -> do
                when verbose $ lift $ putStrLn ("Miss: " ++ toFilePath path)
                misses += 1
                lift $ getSHA1 $ _hRoot header </> path
            Just hsh -> do
                when verbose $ lift $ putStrLn ("Hit: " ++ toFilePath path)
                hits += 1
                return hsh


-- TODO (asayers): reimplement
sanityCheck' :: FileList -> Manifest -> Either String ()
sanityCheck' _fileList _manifest = return ()

-- mkManifest :: Bool -> Maybe Manifest -> Maybe (Path Abs File) -> IO Manifest
-- mkManifest verbose oldManifest excludeFile = do
--     host <- getHostname
--     root <- getWorkingDir
--     ts   <- getCurrentTime
--     either putStrLn return $ sanityCheck oldManifest host root excludeFile ts
--     excludePatterns <- case excludeFile of
--         Nothing -> return []
--         Just p  -> T.lines <$> T.readFile (toFilePath p)

--     stats <- newIORef (Stats 0 0)
--     files <- getFileList root excludePatterns
--     entries <- for files $ \filepath -> (,) filepath <$> do
--         fileModTime <- getModificationTime (toFilePath filepath)
--         let mbOldHash = lookupHash filepath fileModTime =<< oldManifest
--         case mbOldHash of
--             Nothing -> do
--                 when verbose $ putStrLn ("Miss: " ++ toFilePath filepath)
--                 modifyIORef stats $ misses +~ 1
--                 getSHA1 $ root </> filepath
--             Just fileHash -> do
--                 when verbose $ putStrLn ("Hit: " ++ toFilePath filepath)
--                 modifyIORef stats $ hits +~ 1
--                 return fileHash

--     print =<< readIORef stats
--     return $ Manifest host root excludeFile ts (HMS.fromList entries)


-- -- TODO (asayers): If there are warnings we should wait for the user to
-- -- acknowledge before continuing.
-- sanityCheck
--     :: Maybe Manifest -> Hostname -> Path Abs Dir -> Maybe (Path Abs File)
--     -> LastModified -> Either String ()
-- sanityCheck oldManifest host root excludes ts =
--     case oldManifest of
--         Just (Manifest oldHost oldRoot oldExcludes oldTS _) -> do
--             when (oldHost /= host) $ Left "WARNING: hosts don't match"
--             when (oldRoot /= root) $ Left "WARNING: roots don't match"
--             when (oldExcludes /= excludes) $ Left "WARNING: exclude files don't match"
--             when (oldTS >= ts) $ Left "WARNING: existing manifest is newer"
--         Nothing -> return ()



-------------------------------------------------------------------------------
-- Plumbing

subcommandHelp :: IO ()
subcommandHelp = T.putStrLn $ T.unlines
    [ "Usage: mkmanifests <path>"
    , ""
    , "Write a manifest of the working directory tree to the given path. If a"
    , "manifest file already exists at that location, update it."
    , ""
    , "Typical usage:"
    , " $ manifesto mfst"
    , " $ cat mfst | tail -n +6 | while read fhash fpath; do"
    , "       rsync --ignore-existing $fpath \"$HOST:backups/$fhash\""
    , "   done"
    ]

subcommandUpdate :: Bool -> Maybe (Path Rel File) -> Path Rel File -> IO ()
subcommandUpdate verbose excludeFile manifestFile = do
    pwd <- getWorkingDir
    let manFile'     =       pwd </>  manifestFile
    let excludeFile' = fmap (pwd </>) excludeFile

    fileList <- mkFileList pwd excludeFile'

    oldManifests <- maybeToList <$> readManifest manFile'
    case oldManifests of
        [] -> putStrLn $ "Creating new manifest " ++ toFilePath manFile'
        _  -> putStrLn $ "Updating manifest " ++ toFilePath manFile'
    let entryProducer = mkManifest verbose oldManifests fileList
    withSystemTempFile' manFile' $ \pTmp hTmp -> do
        T.hPutStr hTmp (review _Header $ _flHeader fileList)
        T.hPutStr hTmp headerSeparator
        stats <- runEffect $ entryProducer
            >-> P.map ((<>"\n") . review _Entry)
            >-> (fmap (const mempty) (PT.toHandle hTmp))
        hClose hTmp
        copyFile pTmp (toFilePath manFile')
        print stats

-- Open a temporary file in the system temp directory using the given path
-- as a template and perform the given action. If something goes wrong,
-- copy the contents of the file path to the given path, suffixed with
-- a tilde.
withSystemTempFile' :: Path Abs File -> (FilePath -> Handle -> IO a) -> IO a
withSystemTempFile' path fn =
    withSystemTempFile (toFilePath $ filename path) $ \pTmp hTmp ->
        fn pTmp hTmp `onException` do
            hClose hTmp
            copyFile pTmp (toFilePath path <> "~")

-- TODO (asayers): When path is "-", stream to stdout
-- TODO (asayers): Implement "merge" subcommand
main :: IO ()
main = do
    args <- map T.pack <$> getArgs
    let verbose = "-v" `elem` args
        help    = "-h" `elem` args
        excludeFile = listToMaybe $ do
            arg <- args
            Just x <- return $ T.stripPrefix "-e" arg
            Just y <- return $ parseRelFile (T.unpack x)
            return y
        path = listToMaybe $ do
            arg <- args
            guard $ not $ T.isPrefixOf "-" arg
            Just p <- return $ parseRelFile (T.unpack arg)
            return p
    case path of
        _ | help -> subcommandHelp
        Nothing  -> subcommandHelp
        Just p   -> subcommandUpdate verbose excludeFile p
