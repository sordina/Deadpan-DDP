{-# LANGUAGE OverloadedStrings #-}

module Web.DDP.Deadpan.DDP where

-- Internal Imports:

import Data.EJson


-- External Imports:

import qualified Network.WebSockets as WS
import qualified Data.Aeson         as J

-- Types

type Callback a = WS.Connection -> EJsonValue -> IO a


-- Client -->> Server

sendEJ :: WS.Connection -> EJsonValue -> IO ()
sendEJ c = WS.sendTextData c . J.encode . ejson2value

-- Client Connection

clientConnect :: WS.Connection -> IO ()
clientConnect conn = sendEJ conn $
  ejobject [ ("msg",    "connect")
           , ("version", "1")
           , ("support", ejarray ["1","pre2","pre1"]) ]

-- Client Heartbeat

-- TODO: Pong messages have an optional ID attribute in order to match the optional
--       ID attribute of Ping messages...
clientHeartPong :: Maybe String -> WS.Connection -> IO ()
clientHeartPong _ conn = sendEJ conn $ ejobject [ ("msg", "pong") ]

-- Client Data Subscriptions

clientDataSub = undefined
clientDataUnsub = undefined


-- Client RPC

clientRPCMethod = undefined


-- Server -->> Client

getEJ :: WS.Connection -> IO (Maybe EJsonValue)
getEJ = fmap (fmap value2EJson . J.decode) . WS.receiveData

-- Server Data Subscriptions

serverDataNosub       = undefined
serverDataAdded       = undefined
serverDataChanged     = undefined
serverDataRemoved     = undefined
serverDataReady       = undefined
serverDataAddedBefore = undefined
serverDataMovedBefore = undefined


-- Server RPC

serverRPCResult  = undefined
serverRPCUpdated = undefined


-- Server Errors

serverError = undefined
