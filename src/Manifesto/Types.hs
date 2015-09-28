{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Manifesto.Types
    ( Header(..)
    , Entry(..)
    , Exclusions
    , Manifest(..), lookupHash
    , LastModified
    , Hostname, getHostname
    , SHA1(..), getSHA1
    , Stats, hits, misses
    ) where

import Control.Lens
import qualified Crypto.Hash.SHA1 as SHA1
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as BS16
import Data.Hashable
import qualified Data.HashMap.Strict as HMS
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
-- Machinery for logging while computing the new manifest

data Stats = Stats { _hits :: Int, _misses :: Int }
instance Show Stats where
    show (Stats h m) = "hits: " ++ show h ++ ", misses: " ++ show m

instance Monoid Stats where
    mempty = Stats 0 0
    mappend (Stats x1 y1) (Stats x2 y2) = Stats (x1+x2) (y1+y2)

makeLenses ''Stats
