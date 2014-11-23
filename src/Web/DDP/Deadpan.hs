{-# LANGUAGE OverloadedStrings #-}

module Web.DDP.Deadpan where

-- External imports
import           Safe
import           Control.Monad
import           Control.Concurrent     (forkIO)
import           Network.Socket         (withSocketsDo)
import           Data.Text              (Text())
import qualified Network.URI            as U
import qualified Data.Text.IO           as T
import qualified Network.WebSockets     as WS
import qualified Control.Concurrent.STM as STM

-- Internal imports
import Web.DDP.Deadpan.DDP
import Web.DDP.Deadpan.DSL
import Data.EJson

-- TODO: Use better types for these...
type URL = String

-- TODO: Make error handleable
uriError :: String -> IO ()
uriError uri = error $ "Error processing URI [" ++ uri ++ "]"

runURL :: URL -> WS.ClientApp () -> IO ()
runURL uri app = execURI uri app $ getURI uri

getURI :: String -> Maybe (String, Int, String)
getURI uri = do parsed    <- U.parseURI uri
                autho     <- U.uriAuthority parsed
                let port   = readDef 80 $ drop 1 $ U.uriPort autho
                    domain = U.uriRegName autho
                    path   = U.uriPath parsed
                return (domain, port, path)

prop_getURI_full, prop_getURI_missingPort :: Bool
prop_getURI_full        = getURI "http://localhost:1234/testing" == Just ("localhost", 1234, "/testing")
prop_getURI_missingPort = getURI "http://localhost/testing"      == Just ("localhost", 80,   "/testing")

execURI :: String -> WS.ClientApp a -> Maybe (String, Int, String) -> IO ()
execURI _   app (Just (domain, port, path)) = withSocketsDo $ WS.runClient domain port path (setupApp app)
execURI uri _   _                           = uriError uri

dispatch :: WS.Connection -> Maybe EJsonValue -> IO ()
dispatch conn (Just v) = respond conn v
dispatch _    Nothing  = return ()

-- TODO: Remove debugging prints
respond :: WS.Connection -> EJsonValue -> IO ()
respond conn v | v == ejobject [("msg","ping")] = print ("PONGNGNGNGNGNG" :: String) >> print v >> sendpong conn
               | otherwise                      = print v

sendpong :: WS.Connection -> IO ()
sendpong = clientHeartPong Nothing

setupApp :: WS.ClientApp a -> WS.ClientApp ()
setupApp app conn = do
    -- Fork a thread that writes WS data to stdout
    void $ forkIO $ forever $ getEJ conn >>= dispatch conn

    clientConnect conn

    void $ app conn

    -- TODO: Remove this and delegate blocking to App
    void T.getLine -- Block until line entered

    -- TODO: Do we need this?
    WS.sendClose conn ("Bye!" :: Text)

main :: IO ()
main = runURL "websockets://localhost:3000/websocket" (const (return ()))
