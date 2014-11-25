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

  * runDeadpan
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

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Web.DDP.Deadpan.DSL
  ( module Web.DDP.Deadpan.DSL
  , module Data.EJson
  , module Data.EJson.Prism
  , module Data.Text
  )
  where

-- External Imports

import Control.Concurrent.STM
import Control.Concurrent
import Control.Applicative
import Network.WebSockets
import Control.Monad.RWS
import Control.Lens
import Data.Text
import Data.Map

-- Internal Imports

import Web.DDP.Deadpan.Comms
import Data.EJson.Prism
import Data.EJson


-- Let's do this!

type Lookup a = Data.Map.Map Text a

data AppState cb = AppState
  { _defaultCallback :: cb               -- ^ The callback to run when no other callbacks match
  , _callbackSet     :: Lookup cb        -- ^ Callbacks to match against by message
  , _collections     :: TVar EJsonValue  -- ^ Shared data Expected to be an EJObject
  -- , _localState   :: ls               -- ^ Thread-Local state -- TODO: Currently disabled
  }

makeLenses ''AppState

type Callback = EJsonValue -> DeadpanApp () -- TODO: Allow any return type from callback

newtype DeadpanApp a = DeadpanApp
  { _deadpanApp :: Control.Monad.RWS.RWST
                     Network.WebSockets.Connection -- Reader
                     ()                            -- Writer (ignore)
                     (AppState Callback)           -- State
                     IO                            -- Parent Monad
                     a                             -- Result
  }

instance Monad DeadpanApp where
  return  = DeadpanApp . return
  s >>= f = DeadpanApp $ _deadpanApp s >>= _deadpanApp . f

instance Functor DeadpanApp where
  fmap f (DeadpanApp m) = DeadpanApp $ fmap f m

instance Applicative DeadpanApp where
  pure = DeadpanApp . pure
  (DeadpanApp f) <*> (DeadpanApp m) = DeadpanApp (f <*> m)

instance MonadIO DeadpanApp where
  liftIO i = DeadpanApp $ liftIO i

makeLenses ''DeadpanApp

-- | The order of these args match that of runRWST
--
runDeadpan :: DeadpanApp a
           -> Network.WebSockets.Connection
           -> AppState Callback
           -> IO (a, AppState Callback)
runDeadpan app conn appState = do
  (a,s,_w) <- runRWST (_deadpanApp app) conn appState
  return (a,s)

-- TODO: Use a deadpan app in place of a callback
setHandler :: Text -> Callback -> DeadpanApp ()
setHandler k cb = DeadpanApp $ callbackSet %= insert k cb

-- TODO: should I add getHandler/modifyHandler?

deleteHandler :: Text -> DeadpanApp ()
deleteHandler k = DeadpanApp $ callbackSet %= delete k

-- TODO: Once we have stabalised the definition of Callback
--       we can make better use of the 'a' parameter...

setDefaultHandler :: Callback -> DeadpanApp ()
setDefaultHandler cb = DeadpanApp $ defaultCallback .= cb

-- | A low-level function intended to be able to send any arbitrary data to the server.
--   Given that all messages to the server are intended to fit the "message" format,
--   You should probably use `sendMessage` instead.
--   TODO: Decide if this should perform the request in a seperate thread...
sendData :: EJsonValue -> DeadpanApp ()
sendData v = DeadpanApp $ ask >>= liftIO . flip sendEJ v

-- | Send a particular type of message (indicated by the key) to the server.
--   This should be the primary means of [client -> server] communication by
--   a client application.
sendMessage :: Text -> EJsonValue -> DeadpanApp ()
sendMessage key m = sendData messageData
  where
  messageData = ejobject [("msg", ejstring key)] `mappend` m

-- TODO: Consider creating a 'get' instance to handle this...
getAppState :: DeadpanApp (AppState Callback)
getAppState = DeadpanApp $ get

connect :: DeadpanApp ()
connect = sendMessage "connect" $
  ejobject [ ("version", "1")
           , ("support", ejarray ["1","pre2","pre1"]) ]

-- | Provides a way to fork a background thread running the app provided
--   TODO: Consider returning the thread-id
fork :: DeadpanApp a -> DeadpanApp ()
fork app = do
  conn     <- DeadpanApp ask
  appState <- DeadpanApp get
  void $ liftIO $ forkIO $ void $ runDeadpan app conn appState

setup :: DeadpanApp ()
setup = do connect
           fork      $
             forever $ do as      <- getAppState
                          message <- getServerMessage
                          respondToMessage (_callbackSet as) (_defaultCallback as) message

getServerMessage :: DeadpanApp (Maybe EJsonValue)
getServerMessage = DeadpanApp $ ask >>= liftIO . getEJ

respondToMessage :: Lookup Callback -> Callback -> Maybe EJsonValue -> DeadpanApp ()
respondToMessage _     _     Nothing        = return ()
respondToMessage cbSet defCb (Just message) = do
  let maybeMsgName  = message ^? _EJObject "msg" . _EJString
      maybeCallback = do msgName <- maybeMsgName
                         Data.Map.lookup msgName cbSet

  case maybeCallback of Just    cb -> cb    message
                        Nothing    -> defCb message
