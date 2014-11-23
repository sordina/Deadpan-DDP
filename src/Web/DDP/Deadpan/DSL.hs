{-|

  Description : A DSL Designed to make writing Deadpan applications easy!

  This is a simple addition of some application specific functions to

  type DeadpanApp a = Control.Monad.RWS.RWST
                        Network.WebSockets.Connection
                        ()
                        CallbackSet
                        IO
                        a

  A core cabal of functions are exported from this module which are then put to use
  in Web.DDP.Deadpan to create an expressive DSL for creating DDP applications.

  The main functions exported are...

  * runDeadpanWithCallbacks
  * setHandler
  * deleteHandler
  * setDefaultHandler
  * sendData
  * sendMessage

  These allow you to...

  * run a Deadpan application with some initial set of callbacks
  * set new values for response handlers
  * delete existing response handlers
  * set a handler to act when no existing handler matches the incomming message
  * send an EJsonValue to the server (low-level)
  * send messages to be interpreted as RPC calls

  ... respectively.

  There is also a `Control.Lens.Lens` `collections` provided into a single EJsonValue.

  This can be used to...

  * retrieve any current collection data
  * set collection data manually
  * perform actions on collection data in callbacks

-}

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.DDP.Deadpan.DSL where

-- External Imports

import Control.Concurrent.STM
import Network.WebSockets
import Control.Monad.RWS
import Control.Lens
import Data.Text
import Data.Map

-- Internal Imports

import Data.EJson
import Web.DDP.Deadpan.DDP


-- Let's do this!

type CallbackSet = Data.Map.Map Text (Callback ())

data AppState a = AppState { _defaultCallback :: Callback ()     -- ^ The callback to run when no other callbacks match
                           , _callbackSet     :: CallbackSet     -- ^ Callbacks to match against by message
                           , _collections     :: STM EJsonValue  -- ^ Shared data Expected to be an EJObject
                           , _localState      :: a               -- ^ Thread-Local state
                           }

makeLenses ''AppState

type DeadpanApp a s = Control.Monad.RWS.RWST
                        Network.WebSockets.Connection -- Reader
                        ()                            -- Writer (ignore)
                        (AppState s)                  -- State
                        IO                            -- Parent Monad
                        a                             -- Result

-- | The order of these args match that of runRWST
--
runDeadpanWithCallbacks :: DeadpanApp a s
                        -> Network.WebSockets.Connection
                        -> AppState s
                        -> IO (a, AppState s)
runDeadpanWithCallbacks app conn appState = do
  (a,s,_w) <- runRWST app conn appState
  return (a,s)

-- TODO: Use a deadpan app in place of a callback
setHandler :: Text -> Callback () -> DeadpanApp () ()
setHandler k cb = callbackSet . at k .= Just cb

-- TODO: should I add getHandler?

deleteHandler :: Text -> DeadpanApp () ()
deleteHandler = undefined

setDefaultHandler :: Callback a -> DeadpanApp () ()
setDefaultHandler = undefined

-- | A low-level function intended to be able to send any arbitrary data to the server.
--   Given that all messages to the server are intended to fit the "message" format,
--   You should probably use `sendMessage` instead.
sendData :: EJsonValue -> DeadpanApp () ()
sendData = undefined

-- | Send a particular type of message (indicated by the key) to the server.
--   This should be the primary means of [client -> server] communication by
--   a client application.
sendMessage :: Text -> EJsonValue -> DeadpanApp () ()
sendMessage key m = sendData messageData
  where
  messageData = ejobject [("msg", ejstring key)] `mappend` m
