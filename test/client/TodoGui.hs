{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module TodoGui where

import Web.DDP.Deadpan
import System.IO
import Control.Concurrent.STM
import Data.Monoid
import System.Console.ANSI

import qualified Data.Text         as T
import qualified Data.Text.IO      as T
import qualified Data.HashMap.Lazy as M

main :: IO ()
main = do hSetBuffering stdout LineBuffering
          go $ getURI "http://localhost:3000/websocket"

go :: Show a => Either a Params -> IO ()
go (Left  err   ) = print err
go (Right params) = void $ runPingClient params (setSession >> collect >> app)

app :: DeadpanApp String
app = do
  previous <- liftIO $ newTVarIO ejnull
  setCatchAllHandler (dothings previous)
  subscribeWait "publicLists" []
  liftIO getLine

dothings :: TVar EJsonValue -> Callback
dothings v _ = do
  previous <- liftIO $ readTVarIO v
  colls    <- getCollections
  when (previous /= colls) $ void $ do
    liftIO $ clearScreen
    liftIO $ T.putStr $ format (Just colls)
    liftIO $ atomically $ swapTVar v colls

format :: Maybe EJsonValue -> Text
format = T.unlines
       . map vformat
       . M.elems
       . view (pathToTraversal' ["subscription-data", "lists"]
                                . _Just
                                . _EJObject)

vformat :: EJsonValue -> Text
vformat v = " (" <> incomplete <> ") " <> name
  where
  name       = v ^._EJObjectKeyString "name"
  incomplete = maybe "0" (pack . show) (v ^. _EJObjectKey "incompleteCount")
