
{-# LANGUAGE ScopedTypeVariables #-}

module Web.DDP.Deadpan2 where

import Web.DDP.Deadpan.DSL
import Web.DDP.Deadpan.Websockets

-- | Run a DeadpanApp against a URI
--   Returns a `Left` if the initial setup of the client
--   encounters any errors.
--
--   Otherwise, returns a `Right` IO action that can be
--   used to run the client application.
--
--   TODO: This is awful...

runClient :: AppState Callback -> Params -> DeadpanApp a -> IO a
runClient state params app = execURI clientApp params
  where
  clientApp conn = do (a,_as) <- runDeadpan app conn state
                      return a

-- | A client that registers no initial callbacks

bareClient :: AppState Callback
bareClient = undefined


-- | A client that responds to server collection messages

collectiveClient :: AppState Callback
collectiveClient = undefined


-- | A client that logs all server sent messages

loggingClient :: AppState Callback
loggingClient = undefined
