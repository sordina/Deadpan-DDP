
module Main where

import Test.QuickCheck
import System.Process
import System.Exit

main :: IO ()
main = system "cd src && quickcheck Data/EJson/Props.hs" >>= exitWith
