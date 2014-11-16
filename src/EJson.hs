{-# LANGUAGE OverloadedStrings #-}

module EJson where

import Control.Monad
import Data.Aeson
import Data.Scientific
import Data.Text.Internal
import Data.Time
import Data.ByteString
import Data.Vector
import Data.Maybe
import Data.HashMap.Strict

data EJsonValue =
    EJObject !(Data.HashMap.Strict.HashMap Text EJsonValue)
  | EJArray  !(Data.Vector.Vector EJsonValue)
  | EJString !Text
  | EJNumber !Scientific
  | EJBool   !Bool
  | EJDate   !UTCTime
  | EJBinary !ByteString
  | EJUser   !String !Text
  | EJNull
  deriving (Eq, Show)

simpleKey :: Text -> Object -> Maybe Value
simpleKey k = Data.HashMap.Strict.lookup k

parseDate :: Value -> Maybe EJsonValue
parseDate = undefined

parseBinary :: Value -> Maybe EJsonValue
parseBinary = undefined

parseUser :: Value -> Value -> Maybe EJsonValue
parseUser = undefined

parseEscaped :: Value -> Maybe EJsonValue
parseEscaped (Object o) = Just $ simpleObj o
parseEscaped          _ = Nothing

isDate        :: Int -> Object -> Maybe EJsonValue
isDate    1 o  = parseDate =<< simpleKey "$date" o
isDate    _ _  = Nothing
isBinary      :: Int -> Object -> Maybe EJsonValue
isBinary  1 o  = parseBinary =<< simpleKey "$binary" o
isBinary  _ _  = Nothing
isUser        :: Int -> Object -> Maybe EJsonValue
isUser    2 o  = do t <- simpleKey "$type"  o
                    v <- simpleKey "$value" o
                    parseUser t v
isUser    _ _  = Nothing
isEscaped     :: Int -> Object -> Maybe EJsonValue
isEscaped 1 o  = parseEscaped =<< simpleKey "$escape" o
isEscaped _ _  = Nothing

simpleObj :: HashMap Text Value -> EJsonValue
simpleObj o = EJObject $ Data.HashMap.Strict.map value2EJson o

escapeObject :: Object -> EJsonValue
escapeObject o = fromMaybe (simpleObj o)
               $ msum $ Prelude.map ($ o) [isDate l, isBinary l, isUser l, isEscaped l]
  where
  l = Data.HashMap.Strict.size o

value2EJson :: Value -> EJsonValue
value2EJson (Object o) = escapeObject o
value2EJson (Array  a) = EJArray $ Data.Vector.map value2EJson a
value2EJson (String s) = EJString s
value2EJson (Number n) = EJNumber n
value2EJson (Bool   b) = EJBool   b
value2EJson Null       = EJNull
