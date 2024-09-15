{-# LANGUAGE OverloadedStrings #-}

module Lib (Options (..), getOutput) where

import Control.Monad.Except
import Data.Text (pack, replace, unpack)
import System.Exit (ExitCode (..))
import System.Process

getOutput :: [String] -> String -> String -> ExceptT String IO String
getOutput cmd_args input stdin = do
  let cmd = head cmd_args
      args = [(unpack . replace "{input}" (pack input) . pack) word | word <- tail cmd_args]
  (exitc, stdout, err) <- liftIO $ readProcessWithExitCode cmd args stdin
  case exitc of
    ExitFailure _ -> throwError err
    ExitSuccess -> return stdout

data Options = Options
  { varName :: String,
    watchFiles :: [String],
    prompt :: String,
    input :: String,
    useStdin :: Bool,
    command :: String,
    args :: [String]
  }
  deriving (Show)
