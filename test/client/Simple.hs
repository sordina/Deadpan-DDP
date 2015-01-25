{-# LANGUAGE OverloadedStrings #-}
module SimpleClient where
import Web.DDP.Deadpan

main = either print (go app)
              (getURI "http://localhost:3000/websocket")

go app params = runPingClient params (logEverything >> app)

app = do void $ liftIO getLine
         rpcWait "realMethod"    Nothing
         void $ liftIO getLine
         rpcWait "missingMethod" Nothing
         void $ liftIO getLine
