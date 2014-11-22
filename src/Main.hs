module Main (main) where

import System.Environment
import qualified Web.DDP.Deadpan as D

main :: IO ()
main = getArgs >>= go

go :: [String] -> IO ()
go xs | hashelp xs = help
go [url]           = D.runURL url (const (return ())) -- TODO: Create a debugging app to use here...
go _               = help

hashelp :: [String] -> Bool
hashelp xs = any (flip elem xs) (words "-h --help")

help :: IO ()
help = putStrLn "Usage: deadpan [-h | --help] <URL>"
