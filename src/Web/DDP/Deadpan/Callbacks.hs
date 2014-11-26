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
import Control.Monad.State
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

clientDataSub :: Text -> Text -> Maybe [ EJsonValue ] -> DeadpanApp ()
clientDataSub _subid _name _params = undefined

-- | Synonym for `clientDataSub`
subscribe :: Text -> Text -> Maybe [ EJsonValue ] -> DeadpanApp ()
subscribe = clientDataSub

clientDataUnsub :: Text -> DeadpanApp ()
clientDataUnsub _subid = undefined

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

  TODO: * Should the seed actually be able to be an arbitrary value?
        * What is the lens operator to run state against a value?
-}
clientRPCMethod :: Text -> Maybe [EJsonValue] -> Text -> Maybe EJsonValue -> DeadpanApp ()
clientRPCMethod method params rpcid seed = do
  let msg :: [(Text, EJsonValue)]
      msg = [("method", ejstring method), ("id", ejstring rpcid)]
        &~> do maybeM params $ \v -> modify (("params", ejarray  v) :)
               maybeM seed   $ \v -> modify (("seed",            v) :)

  sendMessage "method" (ejobject msg)

  where
  maybeM :: Monad m => Maybe a -> (a -> m ()) -> m ()
  maybeM m f = maybe (return ()) f m

  (&~>) :: s -> State s b -> s
  v &~> m = flip execState v m


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
