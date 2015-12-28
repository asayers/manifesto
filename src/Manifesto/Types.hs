{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Manifesto.Types
    ( Header(..), _Header
    , Entry(..), _Entry
    , Exclusions
    , Manifest(..), mHeader, mEntries
    , LastModified
    , Hostname, getHostname
    , Stats, hits, misses
    , headerParser
    , entryParser
    , manifestPrinter
    ) where

import Control.Lens
import Control.Monad
import Data.Monoid
import qualified Data.Text as T
import Data.Time
import qualified Network.BSD as N
import Path
import Pipes
import Pipes.Parse
import qualified Pipes.Prelude as P
import qualified System.IO as IO

import Manifesto.Hash (SHA1, _SHA1)

-------------------------------------------------------------------------------
-- Types

data Header = Header
    { _hHost            :: !Hostname
    , _hRoot            :: !(Path Abs Dir)
    , _hExcludePatterns :: !Exclusions
    , _hTimestamp       :: !LastModified
    } deriving (Eq, Show)

data Entry = Entry
    { _ePath :: Path Rel File
    , _eHash :: SHA1
    }

type Exclusions = [T.Text] -- TODO (asayers): Make this more flexible

data Manifest m = Manifest
    { _mHeader  :: !Header
    , _mEntries :: !(Producer Entry m ())
    }

type LastModified = UTCTime
type Hostname = T.Text

makeLenses ''Manifest

-- TODO (asayers)
-- rebaseManifest :: Path Abs Dir -> Manifest m -> Manifest m
-- rebaseManifest = undefined

-------------------------------------------------------------------------------
-- Constructors

getHostname :: IO Hostname
getHostname = T.pack <$> N.getHostName

-------------------------------------------------------------------------------
-- Serialisation

_Header :: Prism' T.Text Header
_Header = prism' pp parse
  where
    hostLabel     = "hostname: "
    rootLabel     = "manifest root: "
    excludesLabel = "excludes: "
    tsLabel       = "timestamp: "

    pp (Header host root excludes ts) = T.unlines $
        [ hostLabel <> host
        , rootLabel <> T.pack (toFilePath root)
        , excludesLabel <> T.intercalate ", " excludes
        , tsLabel <> review _LastModified ts
        ]

    parse txt = do
        [hostLn, rootLn, excludesLn, tsLn] <- return (T.lines txt)
        host <- T.stripPrefix hostLabel hostLn
        root <- parseAbsDir . T.unpack =<< T.stripPrefix rootLabel rootLn
        excludes <- T.splitOn "," <$> T.stripPrefix excludesLabel excludesLn
        ts   <- preview _LastModified =<< T.stripPrefix tsLabel tsLn
        return $ Header host root excludes ts

_Entry :: Prism' T.Text Entry
_Entry = prism' pp parse
  where
    pp (Entry path filehash) =
        review _SHA1 filehash <> "\t" <> T.pack (toFilePath path)

    parse txt = do
        [rawHash, path] <- return $ T.splitOn "\t" txt
        filehash <- preview _SHA1 rawHash
        filepath <- parseRelFile (T.unpack path)
        return $ Entry filepath filehash

_LastModified :: Prism' T.Text LastModified
_LastModified = prism'
    (T.pack . formatTime defaultTimeLocale fmt)
    (parseTimeM True defaultTimeLocale fmt . T.unpack)
  where
    fmt = "%F-%X"

manifestPrinter :: Monad m => Manifest m -> Producer T.Text m ()
manifestPrinter (Manifest header entries) = do
    headerPrinter header
    entries >-> P.map (review _Entry)

hHostLabel, hRootLabel, hExcludesLabel, hTsLabel, hSeparator :: T.Text
hHostLabel     = "hostname: "
hRootLabel     = "manifest root: "
hExcludesLabel = "excludes: "
hTsLabel       = "timestamp: "
hSeparator     = "------------"

headerPrinter :: Monad m => Header -> Producer T.Text m ()
headerPrinter (Header host root excludes ts) = do
    yield $ hHostLabel <> host
    yield $ hRootLabel <> T.pack (toFilePath root)
    yield $ hExcludesLabel <> T.intercalate ", " excludes
    yield $ hTsLabel <> review _LastModified ts
    yield $ hSeparator

headerParser :: Monad m => Parser T.Text m (Maybe Header)
headerParser = do
    hostnameTxt  <- draw
    rootTxt      <- draw
    excludesTxt  <- draw
    timestampTxt <- draw
    _separator   <- draw

    let hostname  = T.stripPrefix hHostLabel =<< hostnameTxt
    let root      = parseAbsDir . T.unpack =<< T.stripPrefix hRootLabel =<< rootTxt
    let excludes  = fmap (T.splitOn ",") . T.stripPrefix hExcludesLabel =<< excludesTxt
    let timestamp = preview _LastModified =<< T.stripPrefix hTsLabel =<< timestampTxt

    return $ Header <$> hostname <*> root <*> excludes <*> timestamp

entryParser :: MonadIO m => Pipe T.Text Entry m ()
entryParser = forever $ do
    entryTxt <- await
    case preview _Entry entryTxt of
        Nothing ->
            let err = "Failed to parse entry: " ++ T.unpack entryTxt
            in liftIO $ IO.hPutStrLn IO.stderr err
        Just x ->
            yield x



-------------------------------------------------------------------------------
-- Machinery for logging while computing the new manifest

data Stats = Stats { _hits :: Int, _misses :: Int }
instance Show Stats where
    show (Stats h m) = "hits: " ++ show h ++ ", misses: " ++ show m

instance Monoid Stats where
    mempty = Stats 0 0
    mappend (Stats x1 y1) (Stats x2 y2) = Stats (x1+x2) (y1+y2)

makeLenses ''Stats
