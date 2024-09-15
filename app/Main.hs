
module Main where

import           Control.Exception    (catchJust)
import qualified Data.Attoparsec.Text as P (Parser, char, many', notChar, sepBy)
import           Data.Bifunctor       (first)
import           Data.Either          (fromRight)
import           Data.Function        ((&))
import           Data.Ini             (Ini, keys, lookupValue, parseValue,
                                       readIniFile)
import           Data.Text            (Text, pack, unpack)
import           Lib                  (Options (Options))
import qualified Lib                  as Options (Options (..))
import           Options.Applicative
import           System.Directory     (XdgDirectory (XdgConfig),
                                       getXdgDirectory)
import           System.Exit          (exitFailure)
import           System.FilePath      ((</>))
import           System.IO            (hPutStrLn, stderr)
import           System.IO.Error      (ioeGetErrorType, isDoesNotExistErrorType)
import           UI                   (run)
import qualified Version              (version)

versionOption :: Parser (a -> a)
versionOption = infoOption ("replay version " ++ Version.version)
  (long "version" <> short 'v' <> help "output version information and exit")

options :: Parser Options
options =
  Options
    <$> option
      auto
      ( long "var-name"
          <> short 'n'
          <> help "name of the variable to replace"
          <> showDefault
          <> value "input"
          <> metavar "NAME"
      )
    <*> many
      ( strOption
          ( long "watch"
              <> short 'w'
              <> help "watch FILE and re-run command automatically if it changes"
              <> action "file"
              <> metavar "FILE"
          )
      )
    <*> strOption
    ( long "prompt"
        <> short 'p'
        <> help "prompt to display before input"
        <> showDefault
        <> value ">"
        <> metavar "PROMPT"
    )
    <*> strOption
      ( long "input"
          <> short 'I'
          <> help "input to pass to command"
          <> showDefault
          <> value ""
          <> metavar "INPUT"
      )
    <*> switch
      ( long "from-stdin"
          <> short 'i'
          <> help "read input from stdin, pipe into program continually"
      )
    <*> argument str
      ( metavar "COMMAND"
          <> action "command" )
    <*> many (argument str (metavar "ARGS"))

parseConfig :: FilePath -> IO (Either String Ini)
parseConfig configFile = do
  config <- catchJust (\e -> if isDoesNotExistErrorType (ioeGetErrorType e) then Just () else Nothing)
               (readIniFile configFile)
               (\ _ -> do return $ Right mempty)
  return $ first (\ e -> "error reading " <> configFile <> ": " <> e) config

shlex :: P.Parser [String]
shlex = arg `P.sepBy` space
  where
    space = P.char ' '
    arg = P.many' (P.notChar ' ') <|> quoted
    quoted = P.char '"' *> P.many' (P.notChar '"') <* P.char '"'

main :: IO ()
main = do
  parsedOptions <- execParser opts
  configFile <- getXdgDirectory XdgConfig $ "replay" </> "config.ini"
  config <- parseConfig configFile
  let cmd = pack parsedOptions.command
      cmdOptions = do
        ini <- config
        i <- getIniValue ini cmd "input" lookupValue
        c <- getIniValue ini cmd "command" lookupValue
        a <- getIniValue ini cmd "args" (\ s k -> parseValue s k shlex)
        p <- getIniValue ini cmd "prompt" lookupValue
        return $ parsedOptions &
          modifyOpt a (\ o v -> o { Options.args = v ++ o.args}) &
          modifyOpt i (\ o v -> o { Options.input = unpack v }) &
          modifyOpt c (\ o v -> o { Options.command = unpack v }) &
          modifyOpt p (\ o v -> o { Options.prompt = unpack v})
  case cmdOptions of
    Left e -> do
      hPutStrLn stderr e
      exitFailure
    Right o -> runUI o
  where
    getIniValue :: Ini -> Text -> Text -> (Text -> Text -> Ini -> Either String a) -> Either String (Maybe a)
    getIniValue ini section key f =
      if key `elem` fromRight [] (keys section ini) then
        pure <$> f section key ini
      else
        pure Nothing
    modifyOpt :: Maybe a -> (Options -> a -> Options) -> Options -> Options
    modifyOpt m f o = maybe o (f o) m

    opts =
      info
        (versionOption <*> options <**> helper)
        ( fullDesc
            <> noIntersperse
            <> progDesc "Repeatedly run a command and display its output."
            <> header "replay - interactive command line tool"
        )
    runUI cmdOptions = do
      result <- run cmdOptions
      putStrLn $ unpack result
