{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Lens
import Control.Monad
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Catch (MonadThrow)
import qualified Crypto.Hash.SHA1 as SHA1
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as BS16
import Data.Either
import Data.Hashable
import qualified Data.HashMap.Strict as HMS
import qualified Data.List as List
import Data.Maybe
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import Data.Time (UTCTime(..))
import Data.Traversable
import Path
import System.Directory

-------------------------------------------------------------------------------
-- Configuration

manifestFile :: Path Rel File
manifestFile = $(mkRelFile ".manifest")

-------------------------------------------------------------------------------
-- Types

newtype Manifest = Manifest { unManifest :: HMS.HashMap (Path Rel File) SHA1 }
type LastModified = UTCTime
-- TODO: Use a better type
newtype SHA1 = SHA1 { unHash :: T.Text } -- SHA1

-- TODO: Merge upstream
instance Hashable (Path a b) where
    hashWithSalt s = hashWithSalt s . toFilePath

lookupHash :: Path Rel File -> Manifest -> Maybe SHA1
lookupHash path = HMS.lookup path . unManifest

-------------------------------------------------------------------------------
-- Serialisation

parseManifest :: T.Text -> Maybe Manifest
parseManifest =
    fmap (Manifest . HMS.fromList) . mapM parseManifestEntry . T.lines

parseManifestEntry :: T.Text -> Maybe (Path Rel File, SHA1)
parseManifestEntry txt = do
    let [rawHash, filename] = T.splitOn "\t" txt
    hash <- parseHash rawHash
    filepath <- parseRelFile (T.unpack filename)
    return (filepath, hash)

-- TODO: Check that it looks like a hash
parseHash :: T.Text -> Maybe SHA1
parseHash = Just . SHA1

serialiseManifest :: Manifest -> T.Text
serialiseManifest =
    T.unlines . map serialiseManifestEntry . HMS.toList . unManifest

serialiseManifestEntry :: (Path Rel a, SHA1) -> T.Text
serialiseManifestEntry (filename, hash) =
    serialiseHash hash <> "\t" <> T.pack (toFilePath filename)

serialiseHash :: SHA1 -> T.Text
serialiseHash = unHash

-------------------------------------------------------------------------------
-- Helpers for file IO

readManifest :: Path Abs Dir -> IO (Maybe (Manifest, LastModified))
readManifest dir = runMaybeT $ do
    let path = dir </> manifestFile
    guard =<< lift (doesFileExist $ toFilePath path)
    mbManifest <- lift $ parseManifest <$> T.readFile (toFilePath path)
    modified <- lift $ getModificationTime (toFilePath path)
    let manifest = fromMaybe (error "couldn't parse manifest") mbManifest
    return (manifest, modified)

writeManifest :: Path Abs Dir -> Manifest -> IO ()
writeManifest dir manifest =
    T.writeFile (toFilePath $ dir </> manifestFile) (serialiseManifest manifest)

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

getWorkingDir :: (MonadIO m) => m (Path Abs Dir)
getWorkingDir = liftIO (canonicalizePath "." >>= parseAbsDir)

getFileHash :: Path Abs File -> IO SHA1
getFileHash filepath = do
    digest <- SHA1.hash <$> BS.readFile (toFilePath filepath)
    return $ SHA1 (T.decodeUtf8 $ BS16.encode digest)

-------------------------------------------------------------------------------
-- Logic

-- Returns:
--
-- - Files which are direct children of `dir`, excluding `dir`'s manifest
-- - The manifests of directories which are direct children of `dir`
readDirContents
    :: Path Abs Dir
    -> ([Path Rel Dir], [Path Rel File])
    -> IO [(Path Rel File, LastModified)]
readDirContents dir (dirs0, files0) = do
    let files1 = List.delete manifestFile files0
    let manifests = map (</> manifestFile) dirs0
    let files2 = files1 ++ manifests
    let absFiles = map (dir </>) files2
    modificationTimes <- mapM (getModificationTime . toFilePath) absFiles
    return $ zip files2 modificationTimes

beginningOfTime :: UTCTime
beginningOfTime = UTCTime (toEnum minBound) 0

emptyManifest :: Manifest
emptyManifest = Manifest HMS.empty

mkManifest :: Path Abs Dir -> IO ()
mkManifest dir = do
    mbExistingManifest <- readManifest dir
    let (manifest, manifestModified) =
          fromMaybe (emptyManifest, beginningOfTime) mbExistingManifest
    (dirs, files) <- listDirectory dir
    -- First update the manifests of child directories
    for dirs $ \dirname -> mkManifest $ dir </> dirname
    -- Then compute the hashes of all child files/directory manifests
    entryPaths <- readDirContents dir (dirs, files)
    entries <- for entryPaths $ \(filename, fileModified) -> do
        hash <- maybe (getFileHash $ dir </> filename) return $ do
                    guard (fileModified <= manifestModified)
                    lookupHash filename manifest
        return (filename, hash)
    let manifest' = Manifest $ HMS.fromList entries
    writeManifest dir manifest'

main :: IO ()
main = mkManifest =<< getWorkingDir

