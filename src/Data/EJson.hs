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

  EJson functionality is intended to be used simply by importing `Data.EJson`.

  The internals of EJson are defined in `Data.EJson.EJson`.

  A Prism' instance is defined in `Data.EJson.Prism`.

  Aeson instances are defined in `Data.EJson.Aeson`.

  This module tests examples and properties using DocTest.
-}

{-# LANGUAGE OverloadedStrings #-}

module Data.EJson (

    module Data.EJson.EJson,
    module Data.EJson.EJson2Value,
    matches,
    makeMsg,
    makeId,
    isEJObject,
    isEJArray,
    isEJString,
    isEJNumber,
    isEJBool,
    isEJDate,
    isEJBinary,
    isEJUser,
    isEJNull

  ) where

import Data.EJson.EJson
import Data.EJson.EJson2Value
import Control.Lens
import Data.HashMap.Strict
import Data.Text (Text())



-- | A function to check if all of the values in 'a' match values that exist in 'b'.
--   Not the same as equality.
--
--   { x = L     <=>     ==  { x = L
--   , y = M     <=>     ==  , y = M
--   , z = N     <=>     ==  , z = N
--   }           <=>    ...  , a = ... }
--
--   is still considered as matching.
--
--   Matching is applied recursively.
--
--   Items that are not EJObjects are compared for equality directly.
--
matches :: EJsonValue -> EJsonValue -> Bool
matches a@(EJObject _) b@(EJObject _) = all pairMatches (kvs a)
  where

  kvs (EJObject h) = toList h
  kvs _            = []

  pairMatches (k,v) = case (b ^. _EJObjectKey k)
    of Just x  -> matches v x
       Nothing -> False

matches a b = a == b

-- | Construct a simple message object with no data.
--
makeMsg :: Text -> EJsonValue
makeMsg key = ejobject [("msg", ejstring key)]

-- | Construct a simple object with only an ID.
--
makeId :: Text -> EJsonValue
makeId key = ejobject [("id", ejstring key)]

-- |
-- Examples:
--
-- >>> isEJObject EJNull
-- False

-- | Constructor tests...
isEJObject, isEJArray, isEJString, isEJNumber, isEJBool, isEJDate, isEJBinary, isEJUser, isEJNull :: EJsonValue -> Bool

isEJObject (EJObject _)   = True
isEJObject _              = False
isEJArray  (EJArray _)    = True
isEJArray  _              = False
isEJString (EJString _)   = True
isEJString _              = False
isEJNumber (EJNumber _)   = True
isEJNumber _              = False
isEJBool   (EJBool _)     = True
isEJBool   _              = False
isEJDate   (EJDate _)     = True
isEJDate   _              = False
isEJBinary (EJBinary _)   = True
isEJBinary _              = False
isEJUser   (EJUser _ _)   = True
isEJUser   _              = False
isEJNull   EJNull         = True
isEJNull   _              = False
