module Main where

import           Data.Semigroup      ((<>))
import           Data.Text           (unpack)
import           Lib                 (Options (..))
import           Options.Applicative
import           System.Exit         (exitSuccess)
import           UI                  (run)

versionOption :: Parser (a -> a)
versionOption = infoOption "replay version 0.1" (long "version" <> short 'v' <> help "output version information and exit")

options :: Parser Options
options = Options
      <$> option auto
          ( long "var-name"
         <> short 'n'
         <> help "name of the variable to replace"
         <> showDefault
         <> value "input"
         <> metavar "NAME" )
      <*> some (argument str (metavar "COMMAND ARGS"))

main :: IO ()
main = do
  cmdOptions <- execParser opts
  runUI cmdOptions
  where
    opts = info (versionOption <*> options <**> helper)
      ( fullDesc
     <> noIntersperse
     <> progDesc "Repeatedly run a command and display its output."
     <> header "replay - interactive command line tool" )
    runUI cmdOptions = do
      result <- run cmdOptions
      putStrLn $ unpack result
