
{-# LANGUAGE ScopedTypeVariables #-}

module Web.DDP.Deadpan2 where

import Web.DDP.Deadpan.DSL
import Web.DDP.Deadpan.Websockets
import Data.EJson

import Data.Map
import Control.Concurrent.STM

-- | Run a DeadpanApp against a URI
--   Returns a `Left` if the initial setup of the client
--   encounters any errors.
--
--   Otherwise, returns a `Right` IO action that can be
--   used to run the client application.
--
runClient :: AppState Callback -> Params -> DeadpanApp a -> IO a
runClient state params app = flip execURI params
                  $ \conn -> fmap fst $ runDeadpan app conn state


-- | A client that registers no initial callbacks
--   Note: !!! This does not respond to ping,
--   so you better perform your actions quickly!

bareClient :: IO (AppState Callback)
bareClient = do
  values <- newTVarIO (ejobject [])
  return $ AppState (const $ return ()) empty values


-- | A client that responds to server collection messages

collectiveClient :: AppState Callback
collectiveClient = undefined


-- | A client that logs all server sent messages

loggingClient :: AppState Callback
loggingClient = undefined
