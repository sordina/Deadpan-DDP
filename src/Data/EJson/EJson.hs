{-|

  Description : Internal definitions for EJson functionality

  Currently EJson functionality is built on top of the
  `Data.Aeson.Value` type.

  Functions are written to convert back and forth between
  `Data.EJson.EJsonValue` and `Data.Aeson.Value`.

  The conversion functions from EJsonValue to Value are in a seperate
  module: "Data.EJson.EJson2Value".

  This has some negative impact on performance, but aids simplicity.

-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE     RankNTypes #-}
{-# LANGUAGE     TemplateHaskell #-}
{-# LANGUAGE     OverloadedStrings #-}
{-# LANGUAGE     TypeSynonymInstances #-}
{-# LANGUAGE     MultiParamTypeClasses #-}

module Data.EJson.EJson (

             EJsonValue(..)

             -- Conversion functions
             , value2EJson

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

             -- Prisms
             , _EJObject
             , _EJObjectKey
             , _EJObjectKeyString
             , _EJArray
             , _EJAraryIndex
             , _EJString
             , _EJNumber
             , _EJBool
             , _EJDate
             , _EJBinary
             , _EJUser
             , _EJNull
   ) where

import Control.Monad
import Control.Applicative
import Data.Monoid
import Data.Aeson
import Data.Scientific
import Data.Time.Clock.POSIX
import Data.Time.Clock
import Data.Text.Internal
import Data.Text.Encoding
import Data.ByteString hiding (putStr, map)
import Data.ByteString.Base64
import Data.Maybe
import Data.String
import Control.Lens

import qualified Data.Vector
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T

data EJsonValue =
    EJObject !(HM.HashMap Text EJsonValue)
  | EJArray  !(Data.Vector.Vector EJsonValue)
  | EJString !Text
  | EJNumber !Scientific
  | EJBool   !Bool
  | EJDate   !UTCTime
  | EJBinary !ByteString
  | EJUser   !Text !EJsonValue
  | EJNull
  deriving (Eq)

-- Instances follow!

-- Prismo time!
-- http://adventuretime.wikia.com/wiki/Prismo

makePrisms ''EJsonValue

-- Possibly access the value indicated by a key into a possible EJsonValue EJObject.
--
_EJObjectKey :: Text -> Traversal' EJsonValue (Maybe EJsonValue)
_EJObjectKey k = _EJObject . at k

-- | A helpful prism that looks up values of type EJ{"foo" : "bar", ...}
--   with a Text key "foo" and returns Just "bar", or Nothing.
--   Used frequently for checking message types and ids.
--
_EJObjectKeyString :: Applicative f
                   => Text
                   -> (Text -> f Text)
                   -> EJsonValue
                   -> f EJsonValue
_EJObjectKeyString k = _EJObject . at k . _Just . _EJString

_EJAraryIndex :: Applicative f
              => Int
              -> (EJsonValue -> f EJsonValue)
              -> EJsonValue
              -> f EJsonValue
_EJAraryIndex i = _EJArray . ix i

instance IsString EJsonValue
  where
  fromString = EJString . T.pack

instance Monoid EJsonValue
  where
  mempty = EJNull

  EJObject o1 `mappend` EJObject o2 = EJObject $ mappend o1 o2
  EJArray  a1 `mappend` EJArray  a2 = EJArray  $ mappend a1 a2
  _           `mappend` _           = error "TODO: Haven't considered what to do here yet..."

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

-- Smart Constructors

{-# Inline ejobject #-}
ejobject :: [(Text, EJsonValue)] -> EJsonValue
ejobject = EJObject . HM.fromList

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
ejdate :: Scientific -> EJsonValue
ejdate = EJDate . posixSecondsToUTCTime . realToFrac

{-# Inline ejbinary #-}
ejbinary :: ByteString -> EJsonValue
ejbinary = EJBinary

{-# Inline ejuser #-}
ejuser :: Text -> EJsonValue -> EJsonValue
ejuser = EJUser

{-# Inline ejnull #-}
ejnull :: EJsonValue
ejnull = EJNull


-- Conversion

value2EJson :: Value -> EJsonValue
value2EJson (Object o) = escapeObject o
value2EJson (Array  a) = EJArray $ Data.Vector.map value2EJson a
value2EJson (String s) = EJString s
value2EJson (Number n) = EJNumber n
value2EJson (Bool   b) = EJBool   b
value2EJson Null       = EJNull


-- Helpers

simpleKey :: Text -> Object -> Maybe Value
simpleKey = HM.lookup

parseDate :: Value -> Maybe EJsonValue
parseDate (Number n) = Just $ EJDate $ posixSecondsToUTCTime $ realToFrac n
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

getDate        :: Int -> Object -> Maybe EJsonValue
getDate    1 o  = parseDate =<< simpleKey "$date" o
getDate    _ _  = Nothing
getBinary      :: Int -> Object -> Maybe EJsonValue
getBinary  1 o  = parseBinary =<< simpleKey "$binary" o
getBinary  _ _  = Nothing
getUser        :: Int -> Object -> Maybe EJsonValue
getUser    2 o  = do t <- simpleKey "$type"  o
                     v <- simpleKey "$value" o
                     parseUser t v
getUser    _ _  = Nothing
getEscaped     :: Int -> Object -> Maybe EJsonValue
getEscaped 1 o  = parseEscaped =<< simpleKey "$escape" o
getEscaped _ _  = Nothing

simpleObj :: HM.HashMap Text Value -> EJsonValue
simpleObj o = EJObject $ HM.map value2EJson o

escapeObject :: Object -> EJsonValue
escapeObject o = fromMaybe (simpleObj o) $ msum
               $ map (`uncurry` (HM.size o, o))
                     [getDate, getBinary, getUser, getEscaped]
