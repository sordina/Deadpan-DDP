{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.DDP.Deadpan

main = either print (go app)
              (getURI "http://localhost:3000/websocket")

go app params = runPingClient params (logEverything >> app)

app = do void $ liftIO getLine
         rpcWait "realMethod"    []
         void $ liftIO getLine
         rpcWait "missingMethod" []
         void $ liftIO getLine
