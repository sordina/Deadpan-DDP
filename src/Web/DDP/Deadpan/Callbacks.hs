{-# LANGUAGE OverloadedStrings #-}

module Web.DDP.Deadpan.Callbacks where

import Web.DDP.Deadpan.DSL
import Control.Lens

-- Old Stuff...

-- Client -->> Server

-- Client Heartbeat

pingCallback :: Callback
pingCallback ejv = do
  let mpid = ejv ^? _EJObject "id"
  case mpid of Just pid -> sendMessage "pong" $ ejobject [("id", pid)]
               Nothing  -> sendMessage "pong" $ ejobject []

-- Client Data Subscriptions
-- TODO: Figure out the subscription model

clientDataSub :: Text -> Text -> Maybe [ EJsonValue ] -> DeadpanApp ()
clientDataSub _subid _name _params = undefined

clientDataUnsub :: Text -> DeadpanApp ()
clientDataUnsub _subid = undefined


-- Client RPC

{- | method:     string                        (method name)
     params:     optional array of EJSON items (parameters to the method)
     id:         string                        (an arbitrary client-determined identifier for this method call)
     randomSeed: optional JSON value           (an arbitrary client-determined seed for pseudo-random generators)
-}
clientRPCMethod :: Text -> Maybe [EJsonValue] -> Text -> Maybe EJsonValue -> DeadpanApp ()
clientRPCMethod _method _params _rpcid _seed = undefined


-- Server -->> Client

-- Server Data Subscriptions

serverDataNosub :: Callback
serverDataNosub = undefined

serverDataAdded :: Callback
serverDataAdded = undefined

serverDataChanged :: Callback
serverDataChanged = undefined

serverDataRemoved :: Callback
serverDataRemoved = undefined

serverDataReady :: Callback
serverDataReady = undefined

serverDataAddedBefore :: Callback
serverDataAddedBefore = undefined

serverDataMovedBefore :: Callback
serverDataMovedBefore = undefined


-- Server RPC

serverRPCResult :: Callback
serverRPCResult  = undefined

serverRPCUpdated :: Callback
serverRPCUpdated = undefined


-- Server Errors

serverError :: Callback
serverError = undefined
