module Main where

import           Data.Text           (unpack)
import           Lib                 (Options (..))
import           Options.Applicative
import           UI                  (run)
import qualified Version             (version)

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
    <*> switch
      ( long "from-stdin"
          <> short 'i'
          <> help "read input from stdin, pipe into program continually"
      )
    <*> argument str
      ( metavar "COMMAND"
          <> action "command" )
    <*> some (argument str (metavar "[ARGS]"))

main :: IO ()
main = do
  cmdOptions <- execParser opts
  runUI cmdOptions
  where
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
