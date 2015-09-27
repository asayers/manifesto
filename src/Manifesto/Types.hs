{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Manifesto.Types
    ( Header(..), _Header
    , Entry(..), _Entry
    , Exclusions
    , Manifest(..), _Manifest, lookupHash
    , LastModified
    , Hostname, getHostname
    , SHA1(..), _SHA1, getSHA1
    , Stats, hits, misses
    , headerSeparator
    ) where

import Control.Lens
import qualified Crypto.Hash.SHA1 as SHA1
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as BS16
import Data.Hashable
import qualified Data.HashMap.Strict as HMS
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time
import Path
import System.Process

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

data Manifest = Manifest
    { _mHeader   :: !Header
    , _mEntries  :: !(HMS.HashMap (Path Rel File) SHA1)
    } deriving (Eq, Show)

type LastModified = UTCTime
type Hostname = T.Text

-- TODO: Use a better type
newtype SHA1 = SHA1 { unHash :: T.Text } deriving (Eq, Show)

-- TODO: Merge upstream
instance Hashable (Path a b) where
    hashWithSalt s = hashWithSalt s . toFilePath

lookupHash :: Path Rel File -> Manifest -> Maybe SHA1
lookupHash path = HMS.lookup path . _mEntries

rebaseManifest :: Path Abs Dir -> Manifest -> Manifest
rebaseManifest = undefined

-------------------------------------------------------------------------------
-- Constructors

getHostname :: IO Hostname
getHostname =
    T.concat . T.lines . T.pack <$> readProcess "hostname" ["-s"] ""

getSHA1 :: Path Abs File -> IO SHA1
getSHA1 filepath = do
    digest <- SHA1.hash <$> BS.readFile (toFilePath filepath)
    return $! SHA1 (T.decodeUtf8 $ BS16.encode digest)
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
        let [rawHash, path] = T.splitOn "\t" txt
        filehash <- preview _SHA1 rawHash
        filepath <- parseRelFile (T.unpack path)
        return $ Entry filepath filehash

headerSeparator :: T.Text
headerSeparator = "------------\n"

_Manifest :: Prism' T.Text Manifest
_Manifest = prism' pp parse
  where
    pp m = mconcat
        [ review _Header (_mHeader m)
        , headerSeparator
        , T.unlines (map (review _Entry . uncurry Entry) (HMS.toList (_mEntries m)))
        ]

    parse txt = do
        let (headerTxt, entriesTxt) = T.breakOn headerSeparator txt
        let entriesTxt' = T.drop (T.length headerSeparator) entriesTxt
        let toPair (Entry p h) = (p,h)
        header <- preview _Header headerTxt
        entries <- HMS.fromList <$>
            mapM (fmap toPair . preview _Entry) (T.lines entriesTxt')
        return $ Manifest header entries

_LastModified :: Prism' T.Text LastModified
_LastModified = prism'
    (T.pack . formatTime defaultTimeLocale fmt)
    (parseTimeM True defaultTimeLocale fmt . T.unpack)
  where
    fmt = "%F-%X"

-- TODO: Check that it looks like a hash
_SHA1 :: Prism' T.Text SHA1
_SHA1 = prism' unHash (Just . SHA1)



-------------------------------------------------------------------------------
-- Machinery for logging while computing the new manifest

data Stats = Stats { _hits :: Int, _misses :: Int }
instance Show Stats where
    show (Stats h m) = "hits: " ++ show h ++ ", misses: " ++ show m

instance Monoid Stats where
    mempty = Stats 0 0
    mappend (Stats x1 y1) (Stats x2 y2) = Stats (x1+x2) (y1+y2)

makeLenses ''Stats
