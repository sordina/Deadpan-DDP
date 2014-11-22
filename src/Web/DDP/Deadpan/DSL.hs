{-|
  Description : A DSL Designed to make writing Deadpan applications easy!

  This is a simple addition of some application specific functions to

  type DeadpanApp a = Control.Monad.RWS.RWST
                        Network.WebSockets.Connection
                        ()
                        CallbackSet
                        IO
                        a
-}

module Web.DDP.Deadpan.DSL where

-- External Imports

import Network.WebSockets
import Control.Monad.RWS
import Control.Lens
import Data.Text
import Data.Map

-- Internal Imports

import Web.DDP.Deadpan.DDP


-- Let's do this!

type CallbackSet = Data.Map.Map Text (Callback ())

data AppState = AppState { _defaultCallback :: Callback ()
                         , _callbackSet     :: CallbackSet
                         }

makeLenses ''AppState

type DeadpanApp a = Control.Monad.RWS.RWST
                      Network.WebSockets.Connection -- | Reader
                      ()                            -- | Writer (ignore)
                      AppState                      -- | State
                      IO                            -- | Parent Monad
                      a                             -- | Result

-- | The order of these args match that of runRWST
--
runDeadpanWithCallbacks :: DeadpanApp a -> Network.WebSockets.Connection -> AppState -> IO (a, AppState)
runDeadpanWithCallbacks app conn appState = do
  (a,s,_w) <- runRWST app conn appState
  return (a,s)
