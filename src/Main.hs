module Main (main) where

import System.Environment
import Web.DDP.Deadpan

main :: IO ()
main = getArgs >>= go

go :: [String] -> IO ()
go xs | hashelp xs = help
go [url]           = run $ getURI url
go _               = help

run :: Either Error Params -> IO ()
run (Left  err   ) = print err
run (Right params) = do logger <- loggingClient
                        runClient logger params (liftIO $ void getLine)

hashelp :: [String] -> Bool
hashelp xs = any (flip elem xs) (words "-h --help")

help :: IO ()
help = putStrLn "Usage: deadpan [-h | --help] <URL>"
