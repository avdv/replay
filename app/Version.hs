{-# LANGUAGE TemplateHaskell #-}
module Version (version) where

import           Language.Haskell.TH        (runIO)
import qualified Language.Haskell.TH.Syntax as TH (lift)

version :: String
version = $( let
               firstLine = head . lines
             in runIO (firstLine <$> readFile "VERSION") >>= TH.lift)
