{-# LANGUAGE OverloadedStrings #-}

module Lib (Options (..), getOutput) where

import           Control.Monad.Except
import           Data.Text            (pack, replace, unpack)
import           System.Exit          (ExitCode (..))
import           System.INotify
import           System.Process

getOutput :: [String] -> String -> String -> ExceptT String IO String
getOutput cmdline input stdin = do
  let cmd = head cmdline
      args = [(unpack . replace "input" (pack input) . pack) word | word <- tail cmdline]
  (exitc, stdout, err) <- liftIO $ readProcessWithExitCode cmd args stdin
  case exitc of
    ExitFailure _ -> throwError err
    ExitSuccess   -> return stdout

data Options = Options
  { varName    :: String,
    watchFiles :: [String],
    useStdin   :: Bool,
    cmdline    :: [String]
  }
  deriving (Show)
