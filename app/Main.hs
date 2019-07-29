module Main where

import           Data.Semigroup      ((<>))
import           Options.Applicative
import           UI                  (run)
import           Lib                 (Options(..))

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
  run cmdOptions
  where
    opts = info (options <**> helper)
      ( fullDesc
     <> progDesc "Repeatedly run a command and display its output."
     <> header "repeat - interactive command line tool" )
