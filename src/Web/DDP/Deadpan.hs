{-|

Description: A collection of utilities to provide a way to create and run Deadpan apps.

A collection of utilities to provide a way to create and run Deadpan apps.

This should be the only Deadpan module imported by users intending to use Deadpan as a library
in order to write DDP applications.

-}


{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Web.DDP.Deadpan
  ( module Web.DDP.Deadpan
  , module ReExports
  , getURI
  , Error
  , Params
  , liftIO
  )
  where

import Web.DDP.Deadpan.DSL       as ReExports
import Web.DDP.Deadpan.Callbacks as ReExports
import Control.Monad             as ReExports

import Web.DDP.Deadpan.Websockets

import Data.Maybe
import Control.Concurrent.STM
import Control.Concurrent.Chan
import Control.Monad.IO.Class
import Data.Sequence (empty)

-- | Run a DeadpanApp against a set of connection parameters
--
--   Only runs the app. Does not send connection request. Does not respond to ping!
--
runBareClient :: Params -> DeadpanApp a -> IO a
runBareClient params app = flip execURI params
                $ \conn -> do appState <- newTVarIO $ AppState empty (ejobject []) conn
                              runDeadpan app appState

-- | Run a DeadpanApp after establishing a server conncetion
--
--   Does not respond to ping!
--
runConnectClient :: Params -> DeadpanApp a -> IO a
runConnectClient params app = runBareClient params (fetchMessages >> connect >> app)

-- Same as runConnectClient above but allows specifying version
--
runConnectClientVersion :: Params -> Version -> DeadpanApp a -> IO a
runConnectClientVersion params v app = runBareClient params (fetchMessages >> connectVersion v >> app)

-- | Run a DeadpanApp after registering a ping handler, then establishing a server conncetion.
--
runPingClient :: Params -> DeadpanApp a -> IO a
runPingClient params app = runConnectClient params (handlePings >> app)

-- | Same as runPingClient above but allows specifying version
--
runPingClientVersion :: Params -> Version -> DeadpanApp a -> IO a
runPingClientVersion params v app = runConnectClientVersion params v (handlePings >> app)

-- | Automatically respond to server pings
--
handlePings :: DeadpanApp Text
handlePings = setMsgHandler "ping" pingCallback

-- | Log all incomming messages to STDOUT
--
--   Passes all messages through a Chan in order to not intermingle output lines.
--
--   Returns the chan so that it can be used by other sections of the app.
--
--   Alternatively just set LineBuffering on your output handle.
--
logEverything :: DeadpanApp (Chan String)
logEverything = do pipe <- liftIO newChan
                   _    <- setCatchAllHandler (liftIO . writeChan pipe . show)
                   _    <- fork $ liftIO $ getChanContents pipe >>= mapM_ putStrLn
                   return pipe

-- | A variant of log-everything returning a chan to recieve messages on
--   instead of STDOUT.
logEverythingVia :: DeadpanApp (Chan String)
logEverythingVia = do pipe <- liftIO newChan
                      _    <- setCatchAllHandler (liftIO . writeChan pipe . show)
                      return pipe

-- | A client that responds to server collection messages.
--
--   Warning: this overwrites the "subscription-data" key of the collections field of the AppState.
--
collect :: DeadpanApp ()
collect = void $ setMsgHandler "added"   dataAdded
              >> setMsgHandler "removed" dataRemoved
              >> setMsgHandler "changed" dataChanged

-- | An app to handle the addition of subscription data items...
--
--   For Example: {"collection":"lists","msg":"added","id":"F73xFyAuKrqsb2J3m","fields":{"incompleteCount":6,"name":"Favorite Scientists"}}
--
--   Not especially useful on its own. You would usually use `collect` instead.
--
dataAdded :: Callback
dataAdded           m = fromMaybe (return ()) $ do
  collectionName <- m ^? _EJObjectKeyString "collection"
  itemId         <- m ^? _EJObjectKeyString "id"
  fields         <- m ^. _EJObjectKey       "fields"
  return $ modifyAppState (over collections (putInPath' ["subscription-data", collectionName, itemId] fields))

-- | An app to handle the modification of subscription data items...
--
--   For Example: {"collection":"lists","msg":"changed","id":"TThFzYerrZaxmgjA7","fields":{"name":"List Aasdf"}}
--
--   Not especially useful on its own. You would usually use `collect` instead.
--
dataChanged :: Callback
dataChanged  m = fromMaybe (return ()) $ do
  collectionName <- m ^? _EJObjectKeyString "collection"
  itemId         <- m ^? _EJObjectKeyString "id"
  fields         <- m ^. _EJObjectKey       "fields"
  return $ modifyAppState (over collections (modifyInPath' ["subscription-data", collectionName, itemId] fields))

-- | An app to handle the removal of subscription data items...
--
--   For Example: {"collection":"lists","msg":"removed","id":"By8CtgWGvbZfJPFsd"}
--
--   Not especially useful on its own. You would usually use `collect` instead.
--
dataRemoved :: Callback
dataRemoved  m = fromMaybe (return ()) $ do
  collectionName <- m ^? _EJObjectKeyString "collection"
  itemId         <- m ^? _EJObjectKeyString "id"
  return $ modifyAppState (over collections (removeFromPath' ["subscription-data", collectionName, itemId]))

-- | A helper lens into the subscription data inside the collections section of the dynamic app state.
--
--   Example:
--
--   >>> :set -XOverloadedStrings
--   >>> _collections $ set (subscriptions . _EJObjectKey "songs") (Just ejnull) (AppState undefined ejnull undefined)
--   null
--
subscriptions :: Traversal' (AppState a) EJsonValue
subscriptions = collections . _EJObjectKey "subscription-data" . _Just


-- | A client that sets the session id if the server sends it
--   {"server_id":"83752cf1-a9bf-a15e-b06a-91f110383550"}
--
--   The handler deletes itself when the session is set.
--
setServerID :: DeadpanApp ()
setServerID = do
  hid <- newID
  void $ setHandler hid
       $ \e -> forOf_ (_EJObjectKey "server_id" . _Just) e
       $ \x -> putInBase "server_id" x
            >> deleteHandlerID hid

putInBase :: Text -> EJsonValue -> DeadpanApp ()
putInBase k v = modifyAppState $ set (collections . _EJObjectKey k) (Just v)

-- ensure :: Lens -> Default -> Modifier -> NewValue
-- ensure k v = modifyAppState $ set (collections . _EJObjectKey k) (Just v)

-- | A client that sets the server_id if the server sends it
--   {"msg":"connected","session":"T6gBRv5RpCTwKcMSW"}
--
--   TODO: The handler deletes itself when the session is set.
--
setSession :: DeadpanApp Text
setSession = setMsgHandler "connected" $
       \e -> forOf_ (_EJObjectKey "session" . _Just) e (putInBase "session")
