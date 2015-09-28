{-# LANGUAGE OverloadedStrings #-}

module Manifesto.Serialize
    ( _Header
    , _Entry
    , _Manifest
    , _SHA1
    , headerSeparator
    ) where

import Control.Lens
import Data.Foldable
import qualified Data.HashMap.Strict as HMS
import Data.Monoid
import qualified Data.Text as T
import Data.Time
import Path

import Manifesto.Types

-------------------------------------------------------------------------------
-- Header
-------------------------------------------------------------------------------

-- A fully serialisable Header
data SHeader
    = SHeaderV1 HeaderV1
    | SHeaderV2 HeaderV2

_SHeader :: Prism' T.Text SHeader
_SHeader = prism' pp parse
  where
    pp :: SHeader -> T.Text
    pp val = case val of
        SHeaderV1 x -> review _SHeaderV1 x
        SHeaderV2 x -> review _SHeaderV2 x

    parse :: T.Text -> Maybe SHeader
    parse txt = msum $ map ($ txt) $ reverse
        [ fmap SHeaderV1 . preview _SHeaderV1
        , fmap SHeaderV2 . preview _SHeaderV2
        ]

-------------------------------------------------------------------------------
-- Marshalling

_Header :: Prism' T.Text Header
_Header = prism' pp parse
  where
    pp = review _SHeader . view sHeader
    parse = fmap (review sHeader) . preview _SHeader

sHeader :: Iso' Header SHeader
sHeader = iso serializeHeader deserializeHeader

serializeHeader :: Header -> SHeader
serializeHeader (Header host root exclusions ts) =
    SHeaderV2 (HeaderV2 host root exclusions ts)

deserializeHeader :: SHeader -> Header
deserializeHeader x = case x of
    SHeaderV1 (HeaderV1 host root ts) ->
        Header host root [] ts
    SHeaderV2 (HeaderV2 host root exclusions ts) ->
        Header host root exclusions ts

-------------------------------------------------------------------------------
-- Version 1

data HeaderV1 = HeaderV1 Hostname (Path Abs Dir) LastModified

_SHeaderV1 :: Prism' T.Text HeaderV1
_SHeaderV1 = prism' pp parse
  where
    hostLabel     = "hostname: "
    rootLabel     = "manifest root: "
    tsLabel       = "timestamp: "

    pp (HeaderV1 host root ts) = T.unlines $
        [ hostLabel <> host
        , rootLabel <> T.pack (toFilePath root)
        , tsLabel <> review _LastModified ts
        ]

    parse txt = do
        [hostLn, rootLn, tsLn] <- return (T.lines txt)
        host <- T.stripPrefix hostLabel hostLn
        root <- parseAbsDir . T.unpack =<< T.stripPrefix rootLabel rootLn
        ts   <- preview _LastModified =<< T.stripPrefix tsLabel tsLn
        return $ HeaderV1 host root ts

-------------------------------------------------------------------------------
-- Version 2

data HeaderV2 = HeaderV2 Hostname (Path Abs Dir) Exclusions LastModified

_SHeaderV2 :: Prism' T.Text HeaderV2
_SHeaderV2 = prism' pp parse
  where
    hostLabel     = "hostname: "
    rootLabel     = "manifest root: "
    excludesLabel = "excludes: "
    tsLabel       = "timestamp: "

    pp (HeaderV2 host root excludes ts) = T.unlines $
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
        return $ HeaderV2 host root excludes ts

-------------------------------------------------------------------------------
-- Other types
-------------------------------------------------------------------------------

headerSeparator :: T.Text
headerSeparator = "------------\n"

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

