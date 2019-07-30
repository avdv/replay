{-# LANGUAGE OverloadedStrings #-}
module Lib (Options(..), getOutput) where
import           Control.Monad.Except
import           Data.Text            (pack, replace, unpack)
import           System.Exit          (ExitCode (..))
import           System.Process

getOutput :: [String] -> String -> ExceptT String IO String
getOutput cmdline input = do
  let cmd = head $ cmdline
      args = [ (unpack . replace "input" (pack input) . pack) word | word <- tail cmdline ]
  (exitc, stdout, err) <- liftIO $ readProcessWithExitCode cmd args ""
  case exitc of
    ExitFailure _ -> throwError err
    ExitSuccess   -> return stdout

data Options = Options {
  varName :: String,
  version :: Bool,
  cmdline :: [String]
  }
  deriving(Show)
