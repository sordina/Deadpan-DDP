
module Main where

import Test.DocTest
import System.FilePath.Glob

main :: IO ()
main = do
  sources <- namesMatching "src/**/*.hs"
  doctest $ "-isrc" : sources
