{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Main where

-- Internal
import Vty

-- External
import Web.DDP.Deadpan
import Control.Concurrent.STM
import Control.Concurrent

import qualified Data.HashMap.Lazy as M

main :: IO ()
main = either print start (getURI "http://localhost:3000/websocket")

start :: Params -> IO ()
start params = do
  subs    <- newChan
  lists   <- newChan
  items   <- newChan
  blocker <- newEmptyMVar

  forkIO $ runPingClient params
         $ setSession >> collect
                      >> todoApp blocker subs lists items

  vtyApp subs lists items
  putMVar blocker ()

todoApp :: MVar () -> SubscriptionChan -> ListChan -> ListChan -> DeadpanApp ()
todoApp blocker subs lists items = do
  previousCollections <- liftIO $ newTVarIO ejnull
  currentList         <- liftIO $ newTVarIO Nothing

  setCatchAllHandler $ renderUpdates lists items previousCollections
  subscribeWait "publicLists" []
  fork $ liftIO (getChanContents subs) >>= mapM_ (userInput currentList)

  -- Don't exit the deadpan app until the parent tells us to
  void $ liftIO $ readMVar blocker

userInput :: TVar (Maybe GUID) -> Text -> DeadpanApp ()
userInput currentList s = do
  cl <- liftIO $ readTVarIO currentList
  traverseOf_ _Just unsubscribe cl
  subscribeToList currentList s

subscribeToList :: TVar (Maybe GUID) -> Text -> DeadpanApp ()
subscribeToList cl guid = do
  res <- subscribeWaitId "todos" [ejstring guid]
  case res of Left  err     -> liftIO $ print err
              Right (sid,_) -> liftIO $ atomically $ writeTVar cl (Just sid)

-- NOTE: Updates are written to a chan in order to ensure that
--       they don't clobber each other.
renderUpdates :: ListChan -> ListChan -> TVar EJsonValue -> Callback
renderUpdates lists items v _ = do
  previousCollections <- liftIO $ readTVarIO v
  colls               <- getCollections
  when (previousCollections /= colls) $ void $ do
    liftIO $ do writeChan lists $ formatLists (Just colls)
                writeChan items $ formatTodo  (Just colls)
    liftIO $ atomically $ swapTVar v colls

formatTodo :: Maybe EJsonValue -> ItemList
formatTodo = map formatItem
           . M.toList
           . view (pathToTraversal' ["subscription-data", "todos"]
                                    . _Just
                                    . _EJObject)

formatItem :: (Text,EJsonValue) -> (Text,Text)
formatItem (k,v) = (k, name <> incomplete)
  where
  name       = v ^._EJObjectKeyString "text"
  incomplete = case v ^? _EJObjectKey "checked" . _Just . _EJBool
                 of Just True -> " :)"
                    _         -> " :("

formatLists :: Maybe EJsonValue -> ItemList
formatLists = map formatList
       . M.toList
       . view (pathToTraversal' ["subscription-data", "lists"]
                                . _Just
                                . _EJObject)

formatList :: (Text, EJsonValue) -> (Text, Text)
formatList (k,v) = (k, name <> incomplete)
  where
  name       = v ^._EJObjectKeyString "name"
  incomplete = maybe "0" (surround . pack . show) (v ^. _EJObjectKey "incompleteCount")
  surround x = " (" <> x <> ")"
