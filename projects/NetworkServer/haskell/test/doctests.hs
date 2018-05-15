module Main (main) where

import           System.FilePath.Glob (glob)
import           Test.DocTest         (doctest)

main :: IO ()
main = do
  glob "src/**/*.hs" >>= doctest
