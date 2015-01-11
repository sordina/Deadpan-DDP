{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import Graphics.Vty.Widgets.All
import Graphics.Vty.Input.Events
import Control.Monad
import Data.IORef
import System.Exit

main :: IO ()
main = do
  l1       <- newTextList ["hello", "world"] 1
  l2       <- newList 1
  focus    <- newFocusGroup
  h        <- return l1 <++> vBorder <++> return l2
  coll     <- newCollection

  addToCollection coll h focus

  addToFocusGroup focus l1
  addToFocusGroup focus l2

  addToList l2 'x' =<< plainText "cats"
  addToList l2 'y' =<< plainText "and"
  addToList l2 'z' =<< plainText "boots"

  onKeyPressed focus $ \w k m -> do
    case (k,m) of (KChar 'c', [MCtrl]) -> exitSuccess
                  (KChar 'q', _      ) -> exitSuccess
                  _                    -> return False

  onItemActivated l1 activeItemHandler
  onItemActivated l2 activeItemHandler

  runUi coll defaultContext

activeItemHandler (ActivateItemEvent i a w) = print (i,a)

