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
pingCallback = sendMessage "pong"
             . maybe (ejobject []) makeEJsonId
             . ejson2guid

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
clientDataSub :: GUID -> Text -> [ EJsonValue ] -> DeadpanApp GUID
clientDataSub subid name params = do
  sendMessage "sub" $ makeEJsonId subid
                     <> ejobject [ ("name",   ejstring name)
                                 , ("params", ejarray  params) ]
  return subid

-- | Activates a subscription with an auto-generated ID, returning the ID.
--
subscribe :: Text -> [ EJsonValue ] -> DeadpanApp GUID
subscribe name params = newID >>= \guid -> clientDataSub guid name params

subscribeWaitId :: Text -> [EJsonValue] -> DeadpanApp (Either EJsonValue (GUID, EJsonValue))
subscribeWaitId name params = do
  mv         <- liftIO newEmptyMVar
  subId      <- newID
  handlerIdL <- setMatchHandler (guid2NoSub    subId) (handlerL mv)
  handlerIdR <- setMatchHandler (guid2SubReady subId) (handlerR subId mv)
  _          <- clientDataSub subId name params
  res        <- liftIO $ readMVar mv

  -- Note: This occurs after reading the MVar so it should be safe.
  deleteHandlerID handlerIdR
  deleteHandlerID handlerIdL
  return res

  where
  -- {"msg":"ready","subs":["849d1899-f3af-44b9-919c-7a1ca72c8857"]}
  handlerR subId mv itm = liftIO $ putMVar mv $ Right (subId, itm)
  -- {"error":{...},"msg":"nosub","id":"af0a7ce1-3c37-40d7-8875-b8e3dd737765"}
  handlerL mv itm = forOf_ (_EJObjectKey "error"  . _Just) itm $ liftIO . putMVar mv . Left

subscribeWait :: Text -> [EJsonValue] -> DeadpanApp (Either EJsonValue EJsonValue)
subscribeWait name params = fmap (right' snd)
                                 (subscribeWaitId name params)


{- |
Unsubscribe from an existing subscription indicated by its ID.

@
  unsub (client -> server):
    id: string (the id passed to 'sub')
@
-}
clientDataUnsub :: GUID -> DeadpanApp ()
clientDataUnsub subid = sendMessage "unsub" (makeEJsonId subid)

-- | Synonym for `clientDataUnsub`
unsubscribe :: GUID -> DeadpanApp ()
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
clientRPCMethod :: Text -> Maybe [EJsonValue] -> GUID -> Maybe Text -> DeadpanApp ()
clientRPCMethod method params rpcid seed = do
  let msg = [("method", ejstring method)]
         &~ do forOf_ _Just params $ \v -> modify (("params", ejarray  v):)
               forOf_ _Just seed   $ \v -> modify (("seed",   ejstring v):)

  sendMessage "method" (makeEJsonId rpcid <> ejobject msg)

-- | Like clientRPCMethod, except that it blocks, returning the response from the server.
--
-- TODO: Should we use the seed?
--
rpcWait :: Text -> Maybe [EJsonValue] -> DeadpanApp (Either EJsonValue EJsonValue)
rpcWait method params = do
  mv         <- liftIO newEmptyMVar
  rpcId      <- newID
  handlerId  <- setMatchHandler (makeEJsonId rpcId) (handler mv)
  _          <- clientRPCMethod method params rpcId Nothing
  res        <- liftIO $ readMVar mv

  deleteHandlerID handlerId -- Note: This occurs after reading the MVar so it should be safe.
  return res

  where
  handler mv itm = do forOf_ (_EJObjectKey "error"  . _Just) itm $ liftIO . putMVar mv . Left
                      forOf_ (_EJObjectKey "result" . _Just) itm $ liftIO . putMVar mv . Right

-- Server -->> Client

-- Server Errors

-- TODO
-- serverError :: Callback
-- serverError = undefined
