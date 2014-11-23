{-|

  Description : A collection of Websocket functions to ease converting a URI
                and a Deadpan app into an IO result.

  Intended for internal use.

-}

{-# LANGUAGE OverloadedStrings #-}

module Web.DDP.Deadpan.WebSockets where

-- External imports
import           Safe                   (readDef)
import           Network.Socket         (withSocketsDo)
import           Data.Text              (Text())
import qualified Network.URI            as U
import qualified Network.WebSockets     as WS

-- TODO: Use better types for these...
type URL   = String
type Error = String

runURL :: URL -> WS.ClientApp a -> Either Error (IO a)
runURL uri app = execURI app `fmap` getURI uri

(?>>>) :: Maybe x -> Error -> Either Error x
Just x  ?>>> _ = Right x
Nothing ?>>> e = Left  e

getURI :: String -> Either Error (String, Int, String)
getURI uri = do parsed    <- U.parseURI uri        ?>>> ("Couldn't parse URI [" ++ uri ++ "]")
                autho     <- U.uriAuthority parsed ?>>> ("Couldn't find authority in URI [" ++ show parsed ++ "]")
                let port   = readDef 80 $ drop 1 $ U.uriPort autho
                    domain = U.uriRegName autho
                    path   = U.uriPath parsed
                return (domain, port, path)

prop_getURI_full, prop_getURI_missingPort :: Bool
prop_getURI_full        = getURI "http://localhost:1234/testing" == Right ("localhost", 1234, "/testing")
prop_getURI_missingPort = getURI "http://localhost/testing"      == Right ("localhost", 80,   "/testing")

execURI :: WS.ClientApp a -> (String, Int, String) -> IO a
execURI app (domain, port, path) = withSocketsDo $ WS.runClient domain port path (setupApp app)

setupApp :: WS.ClientApp a -> WS.ClientApp a
setupApp app conn = do
    res <- app conn
    WS.sendClose conn ("Bye!" :: Text) -- TODO: Do we need this?
    return res
