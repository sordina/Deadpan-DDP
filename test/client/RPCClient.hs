{-# LANGUAGE OverloadedStrings #-}

module RPCClient where

import Web.DDP.Deadpan

main :: IO ()
main = go $ getURI "http://localhost:3000/websocket"

go :: Show a => Either a Params -> IO ()
go (Left  err   ) = print err
go (Right params) = do clnt <- loggingClient
                       runClient clnt params app

app :: DeadpanApp ()
app = do clientRPCMethod "testrpcmethod" Nothing "testid" Nothing
         void $ liftIO getLine
