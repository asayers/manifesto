{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

import Control.Lens
import Control.Monad
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Catch (MonadThrow)
import qualified Crypto.Hash.SHA1 as SHA1
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as BS16
import Data.Hashable
import qualified Data.HashMap.Strict as HMS
import Data.IORef
import Data.Maybe
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import Data.Time
import Data.Traversable
import Path
import System.Directory
import System.Environment
import System.IO (hFlush, stdout)
import System.Process


-------------------------------------------------------------------------------
-- Types

data Manifest = Manifest
    { _host      :: !Hostname
    , _root      :: !(Path Abs Dir)
    , _excludes  :: !(Maybe (Path Abs File))
    , _timestamp :: !LastModified
    , _entries   :: !(HMS.HashMap (Path Rel File) SHA1)
    } deriving (Eq, Show)

type LastModified = UTCTime
type Hostname = T.Text

-- TODO: Use a better type
newtype SHA1 = SHA1 { unHash :: T.Text } deriving (Eq, Show)

-- TODO: Merge upstream
instance Hashable (Path a b) where
    hashWithSalt s = hashWithSalt s . toFilePath

lookupHash :: Path Rel File -> LastModified -> Manifest -> Maybe SHA1
lookupHash path fileModTime manifest = do
    guard $ fileModTime <= _timestamp manifest
    HMS.lookup path (_entries manifest)

-------------------------------------------------------------------------------
-- Serialisation

_Manifest :: Prism' T.Text Manifest
_Manifest = prism' pp parse
  where
    hostLabel     = "hostname: "
    rootLabel     = "manifest root: "
    excludesLabel = "excludes file: "
    tsLabel       = "timestamp: "

    pp (Manifest host root excludes ts entries) = T.unlines $
        [ hostLabel <> host
        , rootLabel <> T.pack (toFilePath root)
        , excludesLabel <> T.pack (maybe "" toFilePath excludes)
        , tsLabel <> review _LastModified ts
        , "----------"
        ] ++ map (review _ManifestEntry) (HMS.toList entries)

    parse txt = do
        hostLn : rootLn : excludesLn : tsLn : _ : entryLns <- return (T.lines txt)
        host <- T.stripPrefix hostLabel hostLn
        root <- parseAbsDir . T.unpack =<< T.stripPrefix rootLabel rootLn
        excludes0 <- T.unpack <$> T.stripPrefix excludesLabel excludesLn
        excludes <- if null excludes0
                      then pure Nothing
                      else Just <$> parseAbsFile excludes0
        ts   <- preview _LastModified =<< T.stripPrefix tsLabel tsLn
        entries <- HMS.fromList <$> mapM (preview _ManifestEntry) entryLns
        return $ Manifest host root excludes ts entries

_ManifestEntry :: Prism' T.Text (Path Rel File, SHA1)
_ManifestEntry = prism' pp parse
  where
    pp (path, filehash) =
        review _Hash filehash <> "\t" <> T.pack (toFilePath path)

    parse txt = do
        let [rawHash, path] = T.splitOn "\t" txt
        filehash <- preview _Hash rawHash
        filepath <- parseRelFile (T.unpack path)
        return (filepath, filehash)

_LastModified :: Prism' T.Text LastModified
_LastModified = prism'
    (T.pack . formatTime defaultTimeLocale fmt)
    (parseTimeM True defaultTimeLocale fmt . T.unpack)
  where
    fmt = "%F-%X"

-- TODO: Check that it looks like a hash
_Hash :: Prism' T.Text SHA1
_Hash = prism' unHash (Just . SHA1)

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

getHostname :: IO Hostname
getHostname = oneLine . T.pack <$> readProcess "hostname" ["-s"] ""

oneLine :: T.Text -> T.Text
oneLine = T.concat . T.lines

getWorkingDir :: (MonadIO m) => m (Path Abs Dir)
getWorkingDir = liftIO (canonicalizePath "." >>= parseAbsDir)

getFileHash :: Path Abs File -> IO SHA1
getFileHash filepath = do
    digest <- SHA1.hash <$> BS.readFile (toFilePath filepath)
    return $! SHA1 (T.decodeUtf8 $ BS16.encode digest)

-------------------------------------------------------------------------------
-- Logic for making manifests


data Stats = Stats { _hits :: Int, _misses :: Int }
instance Show Stats where
    show (Stats h m) = "hits: " ++ show h ++ ", misses: " ++ show m

makeLenses ''Stats

-- | TODO: better exclude patterns
getFileList :: Path Abs Dir -> Maybe (Path Abs File) -> IO [Path Rel File]
getFileList root excludeFile = do
    putStr "Calculating file list..."; hFlush stdout
    excludeList <- case excludeFile of
        Nothing -> return []
        Just p  -> T.lines <$> T.readFile (toFilePath p)
    files <- listDirectoryRecursive root
    let files' = filter (not . isExcluded excludeList) files
    putStrLn $ " done (" ++ show (length files') ++ " files)"
    return files'
  where
    isExcluded excludeList path = any
        (\x -> x `T.isPrefixOf` T.pack (toFilePath path))
        (filter (not . T.null) excludeList)


-- TODO: allow multiple existing manifests to be specified
-- TODO: allow existing manifests with different roots; handle properly
-- TODO: this should return `IO (Header, Producer IO Entry)`
mkManifest :: Bool -> Maybe Manifest -> Maybe (Path Abs File) -> IO Manifest
mkManifest verbose oldManifest excludeFile = do
    host <- getHostname
    root <- getWorkingDir
    ts   <- getCurrentTime
    sanityCheck oldManifest host root excludeFile ts

    stats <- newIORef (Stats 0 0)
    files <- getFileList root excludeFile
    entries <- for files $ \filepath -> (,) filepath <$> do
        fileModTime <- getModificationTime (toFilePath filepath)
        let mbOldHash = lookupHash filepath fileModTime =<< oldManifest
        case mbOldHash of
            Nothing -> do
                when verbose $ putStrLn ("Miss: " ++ toFilePath filepath)
                modifyIORef stats $ misses +~ 1
                getFileHash $ root </> filepath
            Just fileHash -> do
                when verbose $ putStrLn ("Hit: " ++ toFilePath filepath)
                modifyIORef stats $ hits +~ 1
                return fileHash

    print =<< readIORef stats
    return $ Manifest host root excludeFile ts (HMS.fromList entries)


-- TODO (asayers): If there are warnings we should wait for the user to
-- acknowledge before continuing.
sanityCheck
    :: Maybe Manifest -> Hostname -> Path Abs Dir -> Maybe (Path Abs File)
    -> LastModified -> IO ()
sanityCheck oldManifest host root excludes ts =
    case oldManifest of
        Just (Manifest oldHost oldRoot oldExcludes oldTS _) -> do
            when (oldHost /= host) $ putStrLn "WARNING: hosts don't match"
            when (oldRoot /= root) $ putStrLn "WARNING: roots don't match"
            when (oldExcludes /= excludes) $ putStrLn "WARNING: exclude files don't match"
            when (oldTS >= ts) $ putStrLn "WARNING: existing manifest is newer"
        Nothing -> return ()



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
    , " $ mkmanifests mfst"
    , " $ cat mfst | while read line; do"
    , "       hash=$(echo $line | cut -d1)"
    , "       path=$(echo $line | cut -d2)"
    , "       rsync --ignore-existing $path \"$HOST:backups/$hash\""
    , "   done"
    ]

-- TODO (asayers): We should write to a file called path~ as we compute the
-- hashes in a streaming fashion. After we're done, we should overwrite
-- path with path~.
subcommandUpdate :: Bool -> Maybe (Path Rel File) -> Path Rel File -> IO ()
subcommandUpdate verbose excludeFile manifestFile = do
    pwd <- getWorkingDir
    let manFile' = pwd </> manifestFile
    oldManifest <- readManifest manFile'
    case oldManifest of
        Nothing -> putStrLn $ "Creating new manifest " ++ toFilePath manFile'
        Just _  -> putStrLn $ "Updating manifest " ++ toFilePath manFile'
    newManifest <- mkManifest verbose oldManifest (fmap (pwd </>) excludeFile)
    writeManifest manFile' newManifest

subcommandStream :: Bool -> Maybe (Path Rel File) -> IO ()
subcommandStream _verbose _excludeFile =
    putStrLn "not implemented"
{-  pwd <- getWorkingDir
    oldManifest <- review _Manifest <$> -- read from stdin
    newManifest <- mkManifest verbose oldManifest
    -- write to stdout
-}


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
