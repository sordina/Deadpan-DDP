{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module TodoGui where

import Web.DDP.Deadpan
import System.IO
import Control.Concurrent.STM
import Control.Concurrent
import System.Console.ANSI

import qualified Data.Text         as T
import qualified Data.Text.IO      as T
import qualified Data.HashMap.Lazy as M

main :: IO ()
main = do hSetBuffering stdout LineBuffering
          either print
                 (\params -> void $ runPingClient params (setSession >> collect >> todoApp))
                 (getURI "http://localhost:3000/websocket")

todoApp :: DeadpanApp ()
todoApp = do
  previousCollections <- liftIO $ newTVarIO ejnull
  currentList         <- liftIO $ newTVarIO Nothing
  ch                  <- liftIO $ sequentialActions

  setCatchAllHandler   $ renderUpdates ch previousCollections
  subscribeWait          "publicLists" []
  liftIO getContents >>= mapM_ (userInput currentList) . lines

sequentialActions :: IO (Chan (IO ()))
sequentialActions = do
  ch <- liftIO $ newChan
  forkIO $ getChanContents ch >>= sequence_
  return ch

userInput :: TVar (Maybe GUID) -> String -> DeadpanApp ()
userInput currentList s = do
  cl <- liftIO $ readTVarIO currentList
  traverseOf_ _Just unsubscribe cl
  colls <- getCollections
  subscribeToList currentList $ getNewList (Just colls)

  where getNewList = lookup s
                   . zip (map (:[]) ['A'..])
                   . M.toList
                   . view (pathToTraversal' ["subscription-data", "lists"]
                                            . _Just
                                            . _EJObject)

subscribeToList :: TVar (Maybe GUID) -> Maybe (Text, EJsonValue) -> DeadpanApp ()
subscribeToList _  Nothing         = return ()
subscribeToList cl (Just (guid,_)) = do
  res <- subscribeWaitId "todos" [ejstring guid]
  case res of Left  err     -> liftIO $ print err
              Right (sid,_) -> liftIO $ atomically $ writeTVar cl (Just sid)

-- NOTE: Updates are written to a chan in order to ensure that
--       they don't clobber each other.
renderUpdates :: Chan (IO ()) -> TVar EJsonValue -> Callback
renderUpdates ch v _ = do
  previousCollections <- liftIO $ readTVarIO v
  colls               <- getCollections
  when (previousCollections /= colls) $ void $ do
    liftIO $ writeChan ch $ do clearScreen
                               T.putStrLn $ formatLists (Just colls)
                               T.putStrLn "---"
                               T.putStrLn $ formatTodo (Just colls)
    liftIO $ atomically $ swapTVar v colls

formatTodo :: Maybe EJsonValue -> Text
formatTodo = T.unlines
           . zipWith formatItem [1..]
           . M.elems
           . view (pathToTraversal' ["subscription-data", "todos"]
                                    . _Just
                                    . _EJObject)

formatItem :: Int -> EJsonValue -> Text
formatItem i v = "[" <> pack (show i) <> "] " <> name <> incomplete
  where
  name       = v ^._EJObjectKeyString "text"
  incomplete = case v ^? _EJObjectKey "checked" . _Just . _EJBool
                 of Just True -> " :)"
                    _         -> " :("

formatLists :: Maybe EJsonValue -> Text
formatLists = T.unlines
       . zipWith formatList ['A'..]
       . M.elems
       . view (pathToTraversal' ["subscription-data", "lists"]
                                . _Just
                                . _EJObject)

formatList :: Char -> EJsonValue -> Text
formatList i v = "[" <> pack [i] <> "] " <> name <> incomplete
  where
  name       = v ^._EJObjectKeyString "name"
  incomplete = maybe "0" (surround . pack . show) (v ^. _EJObjectKey "incompleteCount")
  surround x = " (" <> x <> ")"
