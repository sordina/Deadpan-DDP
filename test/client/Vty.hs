{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Vty where

import qualified Data.Text as T
import Graphics.Vty.Widgets.All
import Graphics.Vty.Input.Events
import Data.IORef
import Data.List
import Data.Maybe
import System.Exit
import Control.Concurrent
import Control.Lens

type SubscriptionChan = Chan T.Text
type ItemList         = [(T.Text, T.Text)]
type ListChan         = Chan ItemList

listUpdater :: ListChan -> Widget (List T.Text FormattedText) -> IO ()
listUpdater chan l = do
  ls <- getChanContents chan
  mapM_ (updateList l) ls

selectByVal :: Widget (List T.Text FormattedText) -> T.Text -> IO ()
selectByVal l val = getIndexByVal l val >>= scrollBy l . pred

getIndexByVal :: Widget (List T.Text FormattedText) -> T.Text -> IO Int
getIndexByVal l val = do
  s     <- getListSize l
  items <- catMaybes `fmap` mapM (getListItem l) [0..s]
  let x  = findIndex ((== val) . fst) items
  return $ fromMaybe 0 x

updateList :: Widget (List T.Text FormattedText) -> ItemList -> IO ()
updateList l1 l = schedule $ do
  -- i <- getSelected l1
  clearList l1
  mapM_ (\(x,y) -> addToList l1 x =<< plainText y) l
  -- forOf_ (_Just . _2 . _1) i (selectByVal l1)

sendKey :: Widget a -> Key -> [Modifier] -> IO Bool
sendKey w k l = do
  w' <- readIORef w
  keyEventHandler w' w k l

vtyApp :: SubscriptionChan -> ListChan -> ListChan -> IO ()
vtyApp subscriptions lists items = do
  l1    <- newTextList [] 1
  l2    <- newList 1
  fg    <- newFocusGroup
  h     <- return l1 <++> vBorder <++> return l2
  coll  <- newCollection

  addToCollection coll h fg

  addToFocusGroup fg l1
  addToFocusGroup fg l2

  -- Add Quit and Vim Keybindings
  onKeyPressed fg $ \_ k m ->
    case (k,m) of (KChar 'c', [MCtrl]) -> exitSuccess
                  (KChar 'q', _      ) -> exitSuccess
                  (KChar 'j', _      ) -> sendKey fg KDown [] >> return True
                  (KChar 'k', _      ) -> sendKey fg KUp   [] >> return True
                  _                    -> return False

  onItemActivated l1 (activeItemHandler subscriptions)
  -- onItemActivated l2 activeItemHandler

  forkIO $ listUpdater lists l1
  forkIO $ listUpdater items l2

  runUi coll defaultContext

activeItemHandler :: Chan a -> ActivateItemEvent a t -> IO ()
activeItemHandler ch (ActivateItemEvent _ a _) = writeChan ch a
