{-# LANGUAGE TemplateHaskell #-}

module Version (version) where

import Data.Maybe (fromMaybe, listToMaybe)
import Language.Haskell.TH (runIO)
import qualified Language.Haskell.TH.Syntax as TH (lift)

version :: String
version =
  $( let firstLine = fromMaybe "unknown" . listToMaybe . lines
      in runIO (firstLine <$> readFile "VERSION") >>= TH.lift
   )
