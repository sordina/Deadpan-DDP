{-# LANGUAGE OverloadedStrings #-}

module DDP where

import EJson
import qualified Network.WebSockets as WS
import qualified Data.Aeson         as J

sendEJ :: WS.Connection -> EJsonValue -> IO ()
sendEJ c = WS.sendTextData c . J.encode . ejson2value

connectMsg :: EJsonValue
connectMsg = ejobject [ ("msg",    "connect")
                      , ("version", "1")
                      , ("support", ejarray ["1","pre2","pre1"]) ]
