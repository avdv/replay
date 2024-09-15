{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Applicative ((<**>), (<|>))
import Control.Exception (catchJust)
import qualified Data.Attoparsec.Text as P (Parser, char, many', notChar, sepBy)
import Data.Bifunctor (first)
import Data.Either (fromRight)
import Data.Function ((&))
import Data.Ini
  ( Ini,
    keys,
    lookupValue,
    parseValue,
    readIniFile,
  )
import Data.Text (Text, pack, unpack)
import Lib (Options (Options))
import qualified Lib as Options (Options (..))
import qualified Options.Applicative as OA
import System.Directory
  ( XdgDirectory (XdgConfig),
    getXdgDirectory,
  )
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)
import System.IO.Error (ioeGetErrorType, isDoesNotExistErrorType)
import UI (run)
import qualified Version (version)

versionOption :: OA.Parser (a -> a)
versionOption =
  OA.infoOption
    ("replay version " ++ Version.version)
    (OA.long "version" <> OA.short 'v' <> OA.help "output version information and exit")

options :: OA.Parser Options
options = do
  varName <-
    OA.option
      OA.auto
      ( OA.long "var-name"
          <> OA.short 'n'
          <> OA.help "name of the variable to replace"
          <> OA.showDefault
          <> OA.value "input"
          <> OA.metavar "NAME"
      )
  watchFiles <-
    OA.many
      ( OA.strOption
          ( OA.long "watch"
              <> OA.short 'w'
              <> OA.help "watch FILE and re-run command automatically if it changes"
              <> OA.action "file"
              <> OA.metavar "FILE"
          )
      )
  prompt <-
    OA.strOption
      ( OA.long "prompt"
          <> OA.short 'p'
          <> OA.help "prompt to display before input"
          <> OA.showDefault
          <> OA.value ">"
          <> OA.metavar "PROMPT"
      )
  input <-
    OA.strOption
      ( OA.long "input"
          <> OA.short 'I'
          <> OA.help "input to pass to command"
          <> OA.showDefault
          <> OA.value ""
          <> OA.metavar "INPUT"
      )
  useStdin <-
    OA.switch
      ( OA.long "from-stdin"
          <> OA.short 'i'
          <> OA.help "read input from stdin, pipe into program continually"
      )
  command <-
    OA.strArgument
      ( OA.metavar "COMMAND"
          <> OA.action "command"
      )
  args <- OA.many (OA.strArgument (OA.metavar "ARGS"))

  return Options {..}

parseConfig :: FilePath -> IO (Either String Ini)
parseConfig configFile = do
  config <-
    catchJust
      (\e -> if isDoesNotExistErrorType (ioeGetErrorType e) then Just () else Nothing)
      (readIniFile configFile)
      (\_ -> do return $ Right mempty)
  return $ first (\e -> "error reading " <> configFile <> ": " <> e) config

shlex :: P.Parser [String]
shlex = arg `P.sepBy` space
  where
    space = P.char ' '
    arg = P.many' (P.notChar ' ') <|> quoted
    quoted = P.char '"' *> P.many' (P.notChar '"') <* P.char '"'

main :: IO ()
main = do
  parsedOptions <- OA.execParser opts
  configFile <- getXdgDirectory XdgConfig $ "replay" </> "config.ini"
  config <- parseConfig configFile
  let cmd = pack parsedOptions.command
      cmdOptions = do
        ini <- config
        i <- getIniValue ini cmd "input" lookupValue
        c <- getIniValue ini cmd "command" lookupValue
        a <- getIniValue ini cmd "args" (\s k -> parseValue s k shlex)
        p <- getIniValue ini cmd "prompt" lookupValue
        return $
          parsedOptions
            & modifyOpt a (\o v -> o {Options.args = v ++ o.args})
            & modifyOpt i (\o v -> o {Options.input = unpack v})
            & modifyOpt c (\o v -> o {Options.command = unpack v})
            & modifyOpt p (\o v -> o {Options.prompt = unpack v})
  case cmdOptions of
    Left e -> do
      hPutStrLn stderr e
      exitFailure
    Right o -> runUI o
  where
    getIniValue :: Ini -> Text -> Text -> (Text -> Text -> Ini -> Either String a) -> Either String (Maybe a)
    getIniValue ini section key f =
      if key `elem` fromRight [] (keys section ini)
        then pure <$> f section key ini
        else pure Nothing
    modifyOpt :: Maybe a -> (Options -> a -> Options) -> Options -> Options
    modifyOpt m f o = maybe o (f o) m

    opts =
      OA.info
        (versionOption <*> options <**> OA.helper)
        ( OA.fullDesc
            <> OA.noIntersperse
            <> OA.progDesc "Repeatedly run a command and display its output."
            <> OA.header "replay - interactive command line tool"
        )
    runUI cmdOptions = do
      result <- run cmdOptions
      putStrLn $ unpack result
