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

import Web.DDP.Deadpan.DSL
import Web.DDP.Deadpan.Websockets
import Web.DDP.Deadpan.Callbacks

import Control.Concurrent.STM
import Control.Concurrent.Chan
import Control.Monad
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

-- | Run a DeadpanApp after registering a ping handler, then establishing a server conncetion.
--
runPingClient :: Params -> DeadpanApp a -> IO a
runPingClient params app = runConnectClient params (fetchMessages >> handlePings >> app)

-- | Automatically respond to server pings
--
handlePings :: DeadpanApp ()
handlePings = setMsgHandler "ping" pingCallback

-- | Log all incomming messages to STDOUT
--
logEverything :: DeadpanApp ()
logEverything = do pipe <- liftIO $ newChan
                   setCatchAllHandler (liftIO . writeChan pipe)
                   void $ fork $ liftIO $ getChanContents pipe >>= mapM_ print

-- | A client that responds to server collection messages.
--
--   TODO: NOT YET IMPLEMENTED
--
collectiveClient :: IO (AppState Callback)
collectiveClient = undefined
