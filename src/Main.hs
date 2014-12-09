{-# LANGUAGE ViewPatterns #-}

module Main (main) where

import Safe
import Data.List
import System.IO
import System.Exit
import Web.DDP.Deadpan
import System.Environment

main :: IO ()
main = getArgs >>= go

go :: [String] -> IO ()
go xs | hashelp xs            = help
go (getVersion -> (v, [url])) = void $ run (getURI url) v
go _                          = help >> exitFailure

-- TODO: No sane person would use this Maybe Maybe monstrosity, but aren't we all a little mad?

run :: Either Error Params -> Maybe (Maybe Version) -> IO String
run (Left  err   ) _               = hPutStrLn stderr err >> exitFailure
run _              (Just Nothing)  = hPutStrLn stderr "Incorrect version specified..." >> exitFailure
run (Right params) (Just (Just v)) = runPingClientVersion params v (logEverything >> liftIO getLine)
run (Right params) Nothing         = runPingClient params (logEverything >> liftIO getLine)

getVersion :: [String] -> (Maybe (Maybe Version), [String])
getVersion ss = (extractVersion ss, deleteVersion ss)

extractVersion :: [String] -> Maybe (Maybe Version)
extractVersion ("-v"        : x : _ ) = Just $ readMay x
extractVersion ("--version" : x : _ ) = Just $ readMay x
extractVersion (              _ : xs) = extractVersion xs
extractVersion _                      = Nothing

deleteVersion :: [String] -> [String]
deleteVersion ("-v"        : _ : xs) = deleteVersion xs
deleteVersion ("--version" : _ : xs) = deleteVersion xs
deleteVersion (              x : xs) = x : deleteVersion xs
deleteVersion xs                     = xs

hashelp :: [String] -> Bool
hashelp xs = any (flip elem xs) (words "-h --help")

help :: IO ()
help = hPutStrLn stderr $ "Usage: deadpan [-h | --help] [ ( -v | --version ) "
    ++ "( " ++ intercalate " | " (map show $ reverse $ [minBound :: Version ..]) ++ " )"
    ++ " ] <URL>"
