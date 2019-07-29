{-# LANGUAGE OverloadedStrings #-}
module Lib (Options(..), getOutput) where
import           Data.Text      (pack, replace, unpack)
import           System.Exit    (ExitCode (..))
import           System.Process

getOutput :: [String] -> String -> IO String
getOutput cmdline input = do
  let cmd = head $ cmdline
      args = [ (unpack . replace "input" (pack input) . pack) word | word <- tail cmdline ]
  (exitc, stdout, err) <- readProcessWithExitCode cmd args ""
  case exitc of
    ExitFailure _ -> error err
    ExitSuccess   -> pure stdout

data Options = Options {
  varName :: String,
  cmdline :: [String]
  }
  deriving(Show)
