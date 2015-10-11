{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Options.Applicative
import Path

import Manifesto.FileSystem
import Manifesto.Logic

data Options = Options
    { excludeFile  :: Maybe (Path Rel File)
    , verbose      :: Bool
    , target       :: Path Rel File
    }

parseOpts :: Parser Options
parseOpts = Options
    <$> optional parseExcludeFile
    <*> parseVerbose
    <*> parseTarget
  where
    parseTarget = argument (eitherReader (maybe (Left "") Right . parseRelFile))
       (metavar "TARGET" <> help "Target manifest file")
    parseExcludeFile = option (eitherReader (maybe (Left "") Right . parseRelFile))
        (long "exclude" <> short 'e' <> help "Path to file containing excludes"
        <> metavar "EXCLUDEFILE")
    parseVerbose = switch
        (long "verbose" <> short 'v' <> help "Whether to be verbose")

helpText :: InfoMod Options
helpText = mconcat
    [ fullDesc
    , progDesc "Update the manifest at TARGET"
    , header "manifesto - incrementally compute the hashes of all files below the working directory"
    ]

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

-- TODO (asayers): When path is "-", stream to stdout
-- TODO (asayers): Implement "merge" subcommand
main :: IO ()
main = do
    opts <- execParser $ info (helper <*> parseOpts) helpText

    pwd <- getWorkingDir
    exclusions <- maybe (return [])
        (fmap T.lines . T.readFile . toFilePath) (excludeFile opts)
    foobarqux (pwd </> target opts) pwd [pwd </> target opts] exclusions
  where
