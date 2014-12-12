{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module RPCClient where

import Web.DDP.Deadpan
import Control.Concurrent.Chan

main :: IO ()
main = go $ getURI "http://localhost:3000/websocket"

go :: Show a => Either a Params -> IO ()
go (Left  err   ) = print err
go (Right params) = void $ runPingClient params (logEverything >>= app)

app :: Chan String -> DeadpanApp String
app chan = do
  liftIO $ putStrLn "Hit Enter to send realMethod and wait for response"
  liftIO getLine
  response <- rpcWait "realMethod" Nothing
  liftIO $ writeChan chan "Got response:"
  liftIO $ writeChan chan (show response)
  unsubscribe "test"
  clientRPCMethod "realMethod"    Nothing "testid1" Nothing
  liftIO getLine
  clientRPCMethod "missingMethod" Nothing "testid2" Nothing
  liftIO getLine
  errorResponse <- rpcWait "missingMethod" Nothing
  liftIO $ writeChan chan "Got response:"
  liftIO $ writeChan chan (show errorResponse)
  liftIO getLine
  clientRPCMethod "realMethod2"   Nothing "testid3" Nothing
  liftIO getLine
