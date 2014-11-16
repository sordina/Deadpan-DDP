{-# LANGUAGE OverloadedStrings #-}

module Deadpan where

import           Control.Concurrent  (forkIO)
import           Control.Monad       (forever, unless)
import           Network.Socket      (withSocketsDo)
import           Data.Text           (Text)
import qualified Data.Text           as T
import qualified Data.Text.IO        as T
import qualified Network.WebSockets  as WS

-- TODO: Use better types for these...
type URL  = String
type Host = String
type Port = Int

runURL :: URL -> IO a
runURL = undefined

run :: Host -> Port -> IO a
run = undefined

app :: WS.ClientApp ()
app conn = do
    putStrLn "Connected!"

    -- Fork a thread that writes WS data to stdout
    _ <- forkIO $ forever $ do
        msg <- WS.receiveData conn
        T.putStrLn msg

    -- Read from stdin and write to WS
    let loop = do
            line <- T.getLine
            unless (T.null line) $ WS.sendTextData conn line >> loop

    loop
    WS.sendClose conn ("Bye!" :: Text)


main :: IO ()
main = withSocketsDo $ WS.runClient "localhost" 3000 "/websocket" app


