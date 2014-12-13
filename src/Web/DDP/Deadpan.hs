{-|

Description: A collection of utilities to provide a way to create and run Deadpan apps.

A collection of utilities to provide a way to create and run Deadpan apps.

This should be the only Deadpan module imported by users intending to use Deadpan as a library
in order to write DDP applications.

-}


{-# LANGUAGE OverloadedStrings #-}

module Web.DDP.Deadpan
  ( module Web.DDP.Deadpan
  , module Web.DDP.Deadpan.DSL
  , module Web.DDP.Deadpan.Callbacks
  , module Control.Monad
  , getURI
  , Error
  , Params
  , liftIO
  )
  where

import Control.Monad.IfElse (awhen)
import Web.DDP.Deadpan.DSL
import Web.DDP.Deadpan.Websockets
import Web.DDP.Deadpan.Callbacks

import Control.Concurrent.STM
import Control.Concurrent.Chan
import Control.Monad
import Control.Lens
import Control.Monad.IO.Class

-- | Run a DeadpanApp against a set of connection parameters
--
--   Only runs the app. Does not send connection request. Does not respond to ping!
--
runBareClient :: Params -> DeadpanApp a -> IO a
runBareClient params app = flip execURI params
                $ \conn -> do appState <- newTVarIO $ AppState [] (ejobject []) conn
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
logEverything = do pipe <- liftIO $ newChan
                   _    <- setCatchAllHandler (liftIO . writeChan pipe . show)
                   _    <- fork $ liftIO $ getChanContents pipe >>= mapM_ putStrLn
                   return pipe

-- | A client that responds to server collection messages.
--
--   TODO: NOT YET IMPLEMENTED
--
collectiveClient :: IO (AppState Callback)
collectiveClient = undefined

-- | A client that sets the session id if the server sends it
--   {"server_id":"83752cf1-a9bf-a15e-b06a-91f110383550"}
--
setServerID :: DeadpanApp ()
setServerID = undefined

putInBase :: Text -> EJsonValue -> DeadpanApp ()
putInBase k v = modifyAppState $ set (collections . _EJObjectKey k) (Just v)

-- | A client that sets the server_id if the server sends it
--   {"msg":"connected","session":"T6gBRv5RpCTwKcMSW"}
--
setSession :: DeadpanApp Text
setSession = setMsgHandler "connected" $
      \e -> awhen (e ^. _EJObjectKey "session")
                  (putInBase "session")
