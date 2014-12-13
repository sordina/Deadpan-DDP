{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module RPCClient where

import Web.DDP.Deadpan
import Control.Concurrent.Chan
import System.IO

main :: IO ()
main = do hSetBuffering stdout LineBuffering
          go $ getURI "http://localhost:3000/websocket"

go :: Show a => Either a Params -> IO ()
go (Left  err   ) = print err
go (Right params) = void $ runPingClient params (setSession >> logEverything >>= app)

app :: Chan String -> DeadpanApp String
app chan = do
  liftIO $ writeChan chan "Connecting to Todos app"
  liftIO $ writeChan chan "Hit enter to quit..."
  newID >>= \guid -> subscribe guid "todos" ["By8CtgWGvbZfJPFsd"]
  newID >>= \guid -> subscribe guid "publicLists" []
  liftIO getLine
