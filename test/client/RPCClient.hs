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
go (Right params) = void $ runPingClient params (setSession >> logEverything >> collect >> app)

app :: DeadpanApp String
app = do
  liftIO $ putStrLn "Hit Enter to send realMethod and wait for response"
  liftIO getLine
  response <- rpcWait "realMethod" Nothing
  liftIO $ putStrLn "Got response:"
  liftIO $ putStrLn (show response)
  newID >>= unsubscribe
  getCollections >>= liftIO . putStrLn . ("Collections Yo: " ++) . show
  testid1 <- newID
  clientRPCMethod "realMethod" Nothing testid1 Nothing
  liftIO getLine
  testid2 <- newID
  clientRPCMethod "missingMethod" Nothing testid2 Nothing
  liftIO getLine
  errorResponse <- rpcWait "missingMethod" Nothing
  liftIO $ putStrLn "Got response:"
  liftIO $ putStrLn (show errorResponse)
  liftIO getLine
  testid3 <- newID
  clientRPCMethod "realMethod2" Nothing testid3 Nothing
  liftIO getLine
