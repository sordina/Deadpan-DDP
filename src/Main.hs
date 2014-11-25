module Main (main) where

import System.IO
import System.Exit
import Web.DDP.Deadpan
import System.Environment

main :: IO ()
main = getArgs >>= go

go :: [String] -> IO ()
go xs | hashelp xs = help
go [url]           = run $ getURI url
go _               = help >> exitFailure

run :: Either Error Params -> IO ()
run (Left  err   ) = hPutStrLn stderr err >> exitFailure
run (Right params) = do logger <- loggingClient
                        runClient logger params (liftIO $ void getLine)

hashelp :: [String] -> Bool
hashelp xs = any (flip elem xs) (words "-h --help")

help :: IO ()
help = hPutStrLn stderr "Usage: deadpan [-h | --help] <URL>"
