{-# LANGUAGE OverloadedStrings #-}

module RPCClient where

import Web.DDP.Deadpan

main :: IO ()
main = go $ getURI "http://localhost:3000/websocket"

go :: Show a => Either a Params -> IO ()
go (Left  err   ) = print err
go (Right params) = do clnt <- loggingClient
                       runClient clnt params app

-- TODO: This doesn't seem to be working completely...
app :: DeadpanApp ()
app = do void $ liftIO getLine
         clientRPCMethod "realMethod"    Nothing "testid1" Nothing
         void $ liftIO getLine
         clientRPCMethod "missingMethod" Nothing "testid2" Nothing
         void $ liftIO getLine
         clientRPCMethod "realMethod2"   Nothing "testid3" Nothing
         void $ liftIO getLine
