module Lib (getOutput) where

import           System.Exit            (ExitCode (..))
import           System.Process

getOutput :: String -> IO String
getOutput input = do
  let cmd = "/home/claudio/.nix-profile/bin/jq"
  let args = ["--color-output", input, "/home/claudio/Downloads/RBMN_API.json"]
  (exitc, stdout, err) <- readProcessWithExitCode cmd args ""
  case exitc of
    ExitFailure _ -> error err
    ExitSuccess   -> pure stdout
