{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Manifesto.Logic
    ( foobarqux
    ) where

import Control.Lens
import Control.Monad
import Control.Monad.Trans.State.Strict
import Data.Function
import qualified Data.List as L
import Data.Maybe
import Data.Monoid
import qualified Data.Text as T
import Data.Time
import Path
import Pipes hiding (for, each)
import qualified Pipes.Prelude as P
import Pipes.Safe
import qualified System.IO as IO

import Manifesto.Types
import Manifesto.FileSystem
import Manifesto.Hash



-- qux :: [Path Abs File]
--     -> Header
--     -> SafeT IO (Pipe (Path Rel File) Entry (SafeT IO) ())
-- qux oldManPaths header = do
--     oldManifests <- mapM readManifest oldManPaths
--     return $ evalStateT (foo header) oldManifests

-- | The paths coming in must be sorted.
createEntries
    :: forall m. MonadSafe m
    => [Manifest m]
    -> Header
    -> Pipe (Path Rel File, LastModified) Entry m ()
createEntries oldManifests0 header =
    flip evalStateT oldManifests0 $ forever $ do
        (filepath, lastModified) <- lift await
        let errMsg e = "error:    " ++ toFilePath filepath ++ ": " ++ show e
        hsh <- catchIOError (Just <$> getHash filepath lastModified)
                (\e -> liftIO (IO.hPutStrLn IO.stderr $ errMsg e) >> return Nothing)
        maybe (return ()) (lift . yield . Entry filepath) hsh
  where
    getHash :: Path Rel File -> LastModified
        -> StateT [Manifest m] (Pipe (Path Rel File, LastModified) Entry m) SHA1
    getHash filepath lastModified = do
        oldManifests <- get
        let absFilepath = _hRoot header </> filepath
        xs <- lift $ lift $ mapM (seekProducer filepath lastModified) oldManifests
        put $ zipWith (set mEntries) (map fst xs) oldManifests
        let existingHash = msum $ map snd xs
        liftIO $ putStrLn $ (if isJust existingHash
                            then "skipping: " else "hashing: ")
                            ++ toFilePath absFilepath
        let computeHash = hashFile absFilepath
        hsh <- maybe computeHash return existingHash
        return hsh

-- | Consume entries from the given producer until we come to:
--
-- - An entry matching the desired path. Return the hash and the remains of
--   the producer.
-- - An entry greater than the desired path. Return Nothing and the remains
--   of the producer, including the final entry to be read.
-- - The end of the producer. Return Nothing and an empty producer.
seekProducer
    :: MonadCatch m
    => Path Rel File
    -> LastModified
    -> Manifest m
    -> m (Producer Entry m (), Maybe SHA1)
seekProducer filepath filets (Manifest header entries)
    | filets > _hTimestamp header =
          return (entries, Nothing)
    | otherwise = go entries
  where
    go es = next es >>= \case
        Left () -> return (es, Nothing)
        Right (e, es') ->
            case (compare `on` toFilePath) (_ePath e) filepath of
                EQ -> return (es', Just (_eHash e))
                GT -> return (es, Nothing)
                LT -> go es'

-- This does a bit of IO immediately, in order to read in the header, but
-- defers reading any of the entries, returning a Producer instead.
readManifest :: MonadSafe m => Path Abs File -> m (Maybe (Manifest m))
readManifest path = do
    (mbHeader, entryLines) <- runStateT headerParser $ readFileLines path
    let entries = entryLines >-> P.filter (not . T.null) >-> entryParser
    return $ (\header -> Manifest header entries) <$> mbHeader

writeManifest :: Path Abs File -> Manifest (SafeT IO) -> SafeT IO ()
writeManifest path manifest = runEffect $
    manifestPrinter manifest >-> writeLinesWithTmpFile path

createHeader :: Path Abs Dir -> Exclusions -> IO Header
createHeader root excludes = do
    host <- getHostname
    timestamp <- getCurrentTime
    return $ Header host root excludes timestamp

-- TODO (asayers): handle old manifests with different roots
-- TODO (asayers): exclusions
createManifest
    :: MonadSafe m
    => Path Abs Dir -> Exclusions -> [Path Abs File]
    -> m (Manifest m)
createManifest rootPath exclusions oldManPaths = do
    oldManifests <- catMaybes <$> mapM readManifest oldManPaths
    header <- liftIO $ createHeader rootPath []
    let entries = regularDescendentsOf rootPath exclusions
          >-> createEntries oldManifests header
    return $ Manifest header entries

foobarqux
    :: Path Abs File -> Path Abs Dir -> [Path Abs File] -> Exclusions -> IO ()
foobarqux outputPath rootPath oldManPaths exclusions = do
    let exclusions' = (map (T.pack (toFilePath rootPath) <>) $
          L.nub $ filter (not . T.null) exclusions)
    print exclusions'
    runSafeT $ do
        manifest <- createManifest rootPath exclusions' oldManPaths
        writeManifest outputPath manifest
