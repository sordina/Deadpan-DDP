
module Main where

import Test.DocTest

main = doctest ["-isrc", "src/Web/DDP/Deadpan.hs", "src/Data/EJson.hs"]
