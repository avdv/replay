module Main where

import           Data.Semigroup      ((<>))
import           Data.Text           (unpack)
import           Lib                 (Options (..))
import           Options.Applicative
import           System.Exit         (exitSuccess)
import           UI                  (run)

showVersion :: IO ()
showVersion = do
  putStrLn "replay version 0.1"
  exitSuccess

options :: Parser Options
options = Options
      <$> option auto
          ( long "var-name"
         <> short 'n'
         <> help "name of the variable to replace"
         <> showDefault
         <> value "input"
         <> metavar "NAME" )
      <*> switch
          ( long "version"
          <> short 'v'
          <> help "output version information and exit")
      <*> some (argument str (metavar "COMMAND [ARGS]"))

main :: IO ()
main = do
  cmdOptions <- execParser opts
  if (version cmdOptions) then showVersion else runUI cmdOptions
  where
    opts = info (options <**> helper)
      ( fullDesc
     <> noIntersperse
     <> progDesc "Repeatedly run a command and display its output."
     <> header "replay - interactive command line tool" )
    runUI cmdOptions = do
      result <- run cmdOptions
      putStrLn $ unpack result
