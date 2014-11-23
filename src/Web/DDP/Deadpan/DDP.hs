{-|

  Description: Higher level abstractions for DDP clients.

  Web.DDP.Deadpan.DDP builds on the abstractions in
  Web.DDP.Deadpan.DSL in order to provide an API intended
  for use by DDP client applications.

  This would be the main module included by client appilcations.

-}

{-# LANGUAGE OverloadedStrings #-}

module Web.DDP.Deadpan.DDP where

-- Internal Imports:

import Data.EJson
import Web.DDP.Deadpan.Comms


-- External Imports:

import qualified Network.WebSockets as WS
import qualified Data.Aeson         as J


-- Client -->> Server

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
