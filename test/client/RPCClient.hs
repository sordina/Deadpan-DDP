{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module RPCClient where

import Web.DDP.Deadpan
import System.IO

main :: IO ()
main = do hSetBuffering stdout LineBuffering
          go $ getURI "http://localhost:3000/websocket"

go :: Show a => Either a Params -> IO ()
go (Left  err   ) = print err
go (Right params) = void $ runPingClient params (setSession >> logEverything >> collectiveClient >> app)

app :: DeadpanApp String
app = do
  liftIO $ putStrLn "Hit Enter to send realMethod and wait for response"
  liftIO getLine
  response <- rpcWait "realMethod" Nothing
  liftIO $ putStrLn "Got response:"
  liftIO $ putStrLn (show response)
  unsubscribe "test"
  getCollections >>= liftIO . putStrLn . ("Collections Yo: " ++) . show
  clientRPCMethod "realMethod"    Nothing "testid1" Nothing
  liftIO getLine
  clientRPCMethod "missingMethod" Nothing "testid2" Nothing
  liftIO getLine
  errorResponse <- rpcWait "missingMethod" Nothing
  liftIO $ putStrLn "Got response:"
  liftIO $ putStrLn (show errorResponse)
  liftIO getLine
  clientRPCMethod "realMethod2"   Nothing "testid3" Nothing
  liftIO getLine
