{-|

Description: Basic communication primitives for DDP.

This module includes the two basic communication primitives required
for a DDP client:

* Sending   (sendEJ)
* Recieving (getEJ)

These functions are included in a seperate module in order
to avoid recursive moldule import issues.

This module is intended for internal use.

-}

module Web.DDP.Deadpan.Comms (sendEJ, getEJ) where

-- Internal Imports:

import Data.EJson

-- External Imports:

import qualified Network.WebSockets as WS
import qualified Data.Aeson         as J

-- Off we go!

-- | Sends an EJsonValue to the server over the connection provided.
sendEJ :: WS.Connection -> EJsonValue -> IO ()
sendEJ c = WS.sendTextData c . J.encode . ejson2value

-- | Possibly gets an EJsonValue from the server over the connection provided
--   TODO: Consider catching exceptions here...
-- getEJ :: WS.Connection -> IO (Maybe EJsonValue)
-- getEJ = fmap (fmap value2EJson . J.decode) . WS.receiveData
--
getEJ :: WS.Connection -> IO (Maybe EJsonValue)
getEJ w = do d <- WS.receiveData w
             print d
             let result = (fmap value2EJson . J.decode) d
             print result
             return result
