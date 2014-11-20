{-|

  Description : Convert between Aeson values and EJson Extended JSON values

  The DDP protocol uses an extended JSON format called EJSON.
  This is embedded inside JSON, so that all JSON is valid EJSON,
  but with certain object structures representing the extended
  types:

  <https://github.com/meteor/meteor/blob/devel/packages/ddp/DDP.md>

  This module provides a pair of functions, `value2EJson` and `ejson2value`
  that convert back and forth between these datatypes. It also provides the
  `EJsonValue` datatype itself.

  Currently there is no implementation of the usual Aeson conversion classes,
  but this may change in the future.

  There are several smart-constructors made available to construct instances
  of EJsonValue more easily. These match the constructors exactly, except for
  substituting lists for vectors, etc... These definitions are inlined.

-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module EJson ( EJsonValue(..)

             -- Conversion functions
             , value2EJson
             , ejson2value

             -- Smart Constructors
             , ejobject
             , ejarray
             , ejstring
             , ejnumber
             , ejbool
             , ejdate
             , ejbinary
             , ejuser
             , ejnull
   ) where

import Control.Monad
import Data.Aeson
import Data.Scientific
import Data.Text.Internal
import Data.Text.Encoding
import Data.ByteString
import Data.Vector
import Data.Maybe
import Data.HashMap.Strict
import Data.ByteString.Base64
import Data.String

-- Time
import Data.Convertible
import System.Posix.Types (EpochTime)

data EJsonValue =
    EJObject !(Data.HashMap.Strict.HashMap Text EJsonValue)
  | EJArray  !(Data.Vector.Vector EJsonValue)
  | EJString !Text
  | EJNumber !Scientific
  | EJBool   !Bool
  | EJDate   !EpochTime
  | EJBinary !ByteString
  | EJUser   !Text !EJsonValue
  | EJNull
  deriving (Eq, Show)

instance IsString EJsonValue
  where
  fromString = EJString . Data.Convertible.convert

-- TODO: Decide what to do about these error cases
instance Num EJsonValue
  where
  fromInteger = EJNumber . fromIntegral
  (EJNumber a) + (EJNumber b) = EJNumber (a + b)
  _            + _            = error "don't add non-numbers"
  (EJNumber a) * (EJNumber b) = EJNumber (a * b)
  _            * _            = error "don't multiply non-numbers"
  abs (EJNumber a)            = EJNumber (abs a)
  abs _                       = error "don't abolute non-numbers"
  signum (EJNumber a)         = EJNumber (signum a)
  signum _                    = error "don't signum non-numbers"
  negate (EJNumber a)         = EJNumber (negate a)
  negate _                    = error "don't negate non-numbers"

instance Convertible EpochTime Scientific
  where
  safeConvert e = Right (Data.Convertible.convert e)

ejson2value :: EJsonValue -> Value
ejson2value (EJObject h    ) = Object (Data.HashMap.Strict.map ejson2value h)
ejson2value (EJArray  v    ) = Array  (Data.Vector.map ejson2value v)
ejson2value (EJString t    ) = String t
ejson2value (EJNumber n    ) = Number n
ejson2value (EJBool   b    ) = Bool b
ejson2value (EJDate   t    ) = makeJsonDate t
ejson2value (EJBinary bs   ) = String $ decodeUtf8 $ Data.ByteString.Base64.encode bs
ejson2value (EJUser   t1 t2) = makeUser t1 t2
ejson2value (EJNull        ) = Null

value2EJson :: Value -> EJsonValue
value2EJson (Object o) = escapeObject o
value2EJson (Array  a) = EJArray $ Data.Vector.map value2EJson a
value2EJson (String s) = EJString s
value2EJson (Number n) = EJNumber n
value2EJson (Bool   b) = EJBool   b
value2EJson Null       = EJNull

-- Smart Constructors

{-# Inline ejobject #-}
ejobject :: [(Text, EJsonValue)] -> EJsonValue
ejobject = EJObject . Data.HashMap.Strict.fromList

{-# Inline ejarray #-}
ejarray :: [EJsonValue] -> EJsonValue
ejarray = EJArray . Data.Vector.fromList

{-# Inline ejstring #-}
ejstring :: Text -> EJsonValue
ejstring = EJString

{-# Inline ejnumber #-}
ejnumber :: Scientific -> EJsonValue
ejnumber = EJNumber

{-# Inline ejbool #-}
ejbool :: Bool -> EJsonValue
ejbool = EJBool

{-# Inline ejdate #-}
ejdate :: EpochTime -> EJsonValue
ejdate = EJDate

{-# Inline ejbinary #-}
ejbinary :: ByteString -> EJsonValue
ejbinary = EJBinary

{-# Inline ejuser #-}
ejuser :: Text -> EJsonValue -> EJsonValue
ejuser = EJUser

{-# Inline ejnull #-}
ejnull :: EJsonValue
ejnull = EJNull


-- Helpers

simpleKey :: Text -> Object -> Maybe Value
simpleKey k = Data.HashMap.Strict.lookup k

integer2date :: Integer -> EpochTime
integer2date = Data.Convertible.convert

parseDate :: Value -> Maybe EJsonValue
parseDate (Number n) = Just $ EJDate $ integer2date $ round n
parseDate _          = Nothing

parseBinary :: Value -> Maybe EJsonValue
parseBinary (String s) = Just (EJBinary (decodeLenient (encodeUtf8 s)))
parseBinary _          = Nothing

parseUser :: Value -> Value -> Maybe EJsonValue
parseUser (String k) v = Just $ EJUser k (value2EJson v)
parseUser _          _ = Nothing

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

makeJsonDate :: EpochTime -> Value
makeJsonDate t = Object
               $ Data.HashMap.Strict.fromList
               [ ("$date", Number $ Data.Convertible.convert t) ]

makeUser :: Text -> EJsonValue -> Value
makeUser t v = Object
           $ Data.HashMap.Strict.fromList
           [ ("$type" , String t)
           , ("$value", ejson2value v)]
