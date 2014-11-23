
module Web.DDP.Deadpan2 where

import Web.DDP.Deadpan.DSL


-- | A client that registers no initial callbacks

bareClient :: DeadpanApp a -> DeadpanApp a
bareClient = undefined


-- | A client that responds to server collection messages

collectiveClient :: DeadpanApp a -> DeadpanApp a
collectiveClient = undefined


-- | A client that logs all server sent messages

loggingClient :: DeadpanApp a -> DeadpanApp a
loggingClient = undefined
