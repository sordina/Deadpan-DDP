{-|

Description: Intended to provide a set of callbacks for various server events.

This module is intended to provide a set of callbacks for various server events.

The set of callbacks provided fulfills the functionality require to be able
to implement a local data-store reflecting server-sent data-update messages.

"Web.DDP.Deadpan.Callbacks" is used frequently in "Web.DDP.Deadpan".

-}

{-# LANGUAGE OverloadedStrings #-}

module Web.DDP.Deadpan.Callbacks where

import Web.DDP.Deadpan.DSL
import Control.Concurrent.MVar
import Control.Monad.State

-- Old Stuff...

-- Client -->> Server

-- Client Heartbeat

pingCallback :: Callback
pingCallback ejv = do
  let mpid = ejv ^. _EJObjectKey "id"
  case mpid of Just pid -> sendMessage "pong" $ ejobject [("id", pid)]
               Nothing  -> sendMessage "pong" $ ejobject []

-- Client Data Subscriptions


{- |

Initiate a subscription to a named collection on the server.

Provide an id to refer to the subscription in future.

@
  sub (client -> server):
    id:     string                        (an arbitrary client-determined
                                              identifier for this subscription)
    name:   string                        (the name of the subscription)
    params: optional array of EJSON items (parameters to the subscription)
@

-}
clientDataSub :: Text -> Text -> [ EJsonValue ] -> DeadpanApp ()
clientDataSub subid name params
  = sendMessage "sub" $ ejobject [("name",   ejstring name)
                                 ,("params", ejarray  params)
                                 ,("id",     ejstring subid)]

-- | Activates a subscription with an auto-generated ID, returning the ID.
--
subscribe :: Text -> [ EJsonValue ] -> DeadpanApp ()
subscribe name params = newID >>= \guid -> clientDataSub guid name params

subscribeWait :: Text -> [EJsonValue] -> DeadpanApp (Either EJsonValue EJsonValue)
subscribeWait name params = do
  mv         <- liftIO newEmptyMVar
  subId      <- newID
  handlerIdL <- setMatchHandler (makeNoSub    subId) (handlerL mv)
  handlerIdR <- setMatchHandler (makeSubReady subId) (handlerR mv)
  _          <- clientDataSub subId name params
  res        <- liftIO $ readMVar mv

  -- Note: This occurs after reading the MVar so it should be safe.
  deleteHandlerID handlerIdR
  deleteHandlerID handlerIdL
  return res

  where
  -- {"msg":"ready","subs":["849d1899-f3af-44b9-919c-7a1ca72c8857"]}
  handlerR mv itm = liftIO $ putMVar mv $ Right itm
  -- {"error":{...},"msg":"nosub","id":"af0a7ce1-3c37-40d7-8875-b8e3dd737765"}
  handlerL mv itm = forOf_ (_EJObjectKey "error"  . _Just) itm $ liftIO . putMVar mv . Left


{- |
Unsubscribe from an existing subscription indicated by its ID.

@
  unsub (client -> server):
    id: string (the id passed to 'sub')
@
-}
clientDataUnsub :: Text -> DeadpanApp ()
clientDataUnsub subid = sendMessage "unsub" $ ejobject [("id", ejstring subid)]

-- | Synonym for `clientDataUnsub`
unsubscribe :: Text -> DeadpanApp ()
unsubscribe = clientDataUnsub


-- Client RPC

{- |
  As explained in the Meteor DDP documentation:

  @
      method:     string                        (method name)
      params:     optional array of EJSON items (parameters to the method)
      id:         string                        (an arbitrary client-determined identifier for this method call)
      randomSeed: optional JSON value           (an arbitrary client-determined seed for pseudo-random generators)
  @
-}
clientRPCMethod :: Text -> Maybe [EJsonValue] -> Text -> Maybe Text -> DeadpanApp ()
clientRPCMethod method params rpcid seed = do
  let msg = [("method", ejstring method), ("id", ejstring rpcid)]
         &~ do forOf_ _Just params $ \v -> modify (("params", ejarray  v):)
               forOf_ _Just seed   $ \v -> modify (("seed",   ejstring v):)

  sendMessage "method" (ejobject msg)

-- | Like clientRPCMethod, except that it blocks, returning the response from the server.
--
-- TODO: Should we use the seed?
--
rpcWait :: Text -> Maybe [EJsonValue] -> DeadpanApp (Either EJsonValue EJsonValue)
rpcWait method params = do
  mv         <- liftIO newEmptyMVar
  rpcId      <- newID
  handlerId  <- setMatchHandler (makeId rpcId) (handler mv)
  _          <- clientRPCMethod method params rpcId Nothing
  res        <- liftIO $ readMVar mv

  deleteHandlerID handlerId -- Note: This occurs after reading the MVar so it should be safe.
  return res

  where
  handler mv itm = do forOf_ (_EJObjectKey "error"  . _Just) itm $ liftIO . putMVar mv . Left
                      forOf_ (_EJObjectKey "result" . _Just) itm $ liftIO . putMVar mv . Right

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
