{-# LANGUAGE ViewPatterns #-}

module Main (main) where

import Safe
import Data.List
import System.IO
import System.Exit
import Web.DDP.Deadpan
import System.Environment
import Data.Aeson
import Data.Maybe
import Control.Concurrent.Chan
import Data.EJson.Aeson -- TODO: Aeson instance should come with EJson import
import qualified Data.ByteString.Lazy.Char8 as C8
import qualified System.Console.Haskeline   as R

main :: IO ()
main = getArgs >>= go

go :: [String] -> IO ()
go xs | hashelp xs            = help
go (getVersion -> (v, [url])) = void $ run (getURI url) v
go _                          = help >> exitFailure

-- TODO: No sane person would use this Maybe Maybe monstrosity, but aren't we all a little mad?

run :: Either Error Params -> Maybe (Maybe Version) -> IO ()
run (Left  err   ) _               = hPutStrLn stderr err >> exitFailure
run _              (Just Nothing)  = hPutStrLn stderr "Incorrect version specified..." >> exitFailure
run (Right params) (Just (Just v)) = runPingClientVersion params v (logEverything >> sendMessages)
run (Right params) Nothing         = runPingClient        params   (logEverything >> sendMessages)

-- TODO: Allow a full DSL to be used rather than just messages?
--
sendMessages :: DeadpanApp ()
sendMessages = do
  c <- liftIO $ newChan
  let settings = R.defaultSettings { R.autoAddHistory = True }
  void $ fork $ liftIO $ R.runInputT settings (inOutLoop c)
  contents <- liftIO (getChanContents c)
  mapM_ sendPossibleMessage (catMaybes (takeWhile isJust contents))

inOutLoop :: Chan (Maybe String) -> R.InputT IO ()
inOutLoop c = do
  maybeLine <- R.getInputLine ""
  case maybeLine of
    Nothing      -> liftIO $ writeChan c Nothing -- EOF / control-d
    Just ":exit" -> liftIO $ writeChan c Nothing
    Just line    -> do liftIO $ writeChan c (Just line)
                       inOutLoop c

sendPossibleMessage :: String -> DeadpanApp ()
sendPossibleMessage msgStr = do
  let decoded = decode $ C8.pack msgStr
  case decoded of Just m  -> sendData m
                  Nothing -> liftIO $ print "Invalid Message"

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
