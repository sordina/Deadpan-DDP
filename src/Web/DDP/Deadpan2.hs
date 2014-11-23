
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

runURI :: String -> DeadpanApp a -> Either String (IO a)
runURI uri client = extract `fmap` runURL uri app
  where
  app conn = runDeadpan client conn undefined
  extract act = do (a,_) <- act
                   return a


-- | A client that registers no initial callbacks

bareClient :: DeadpanApp a -> DeadpanApp a
bareClient = undefined


-- | A client that responds to server collection messages

collectiveClient :: DeadpanApp a -> DeadpanApp a
collectiveClient = undefined


-- | A client that logs all server sent messages

loggingClient :: DeadpanApp a -> DeadpanApp a
loggingClient = undefined
