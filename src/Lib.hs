{-# LANGUAGE OverloadedStrings #-}

module Lib (Options (..), getOutput) where

import Control.Monad.Except
import Control.Monad.IO.Class (liftIO)
import Data.Text (pack, replace, unpack)
import System.Exit (ExitCode (..))
import System.Process

getOutput :: String -> [String] -> String -> String -> ExceptT String IO String
getOutput cmd args input stdin = do
  let args' = [(unpack . replace "{input}" (pack input) . pack) word | word <- args]
  (exitc, stdout, err) <- liftIO $ readProcessWithExitCode cmd args' stdin
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
