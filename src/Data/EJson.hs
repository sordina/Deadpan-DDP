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

  Aeson instances are defined in `Data.EJson.Aeson`.

  This module tests examples and properties using DocTest.
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Data.EJson (

    module Data.EJson.EJson,
    module Data.EJson.EJson2Value,
    module Control.Lens,

    matches,
    putInPath,
    putInPath',
    modifyInPath,
    modifyInPath',
    removeFromPath,
    removeFromPath',

    makeMsg,
    makeId,
    makeSubReady,
    makeNoSub,

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
import Control.Monad.State (execState)
import Data.Text (Text())

import qualified Data.HashMap.Strict as HM


-- | A function to check if all of the values in 'a' match values that exist in 'b'.
--   Not the same as equality.
--
--   @
--       { x = L             ==  { x = L
--       , y = M     <=>     ==  , y = M
--       , z = N             ==  , z = N
--       }                  ...  , a = ... }
--   @
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

  kvs (EJObject h) = HM.toList h
  kvs _            = []

  pairMatches (k,v) = case b ^. _EJObjectKey k
    of Just x  -> matches v x
       Nothing -> False

matches a b = a == b

-- | putInPath is a method for placing a value into an EJsonValue object at a point indicated by a path
--   The path is a list of text values indicating successive object keys.
--   This can't be done with simple lenses, as the nested obects may not exist.
--   If they do exist, then it is simply an update.
--   However, if they don't exist then EJObjects are created during the traversal.
--
--   Examples:
--
--   >>> :set -XOverloadedStrings
--
--   >>> putInPath ["a"] "b" (ejobject [("x","y")])
--   Right {"a":"b","x":"y"}
--
--   >>> putInPath ["a","q","r"] (ejobject [("s","t")]) (ejobject [("x","y")])
--   Right {"a":{"q":{"r":{"s":"t"}}},"x":"y"}
--
--   If you attempt to update a value as if it were an EJObject when in-fact it is something else,
--   then you will receive an Error.
--
--   Example:
--
--   >>> putInPath ["a", "b"] "c" (ejobject [("a","hello")])
--   Left "Value \"hello\" does not match path [\"b\"]."
--
putInPath :: [Text] -> EJsonValue -> EJsonValue -> Either String EJsonValue

putInPath [] payload _ = Right payload

putInPath (h:t) payload target@(EJObject _) =
  let l = _EJObjectKey h
   in case target ^. l
      of Nothing -> Right $ set l (Just (expand t payload)) target
         Just v  -> do r <- putInPath t payload v
                       Right $ set l (Just r) target

putInPath path _ target = Left (concat ["Value ", show target, " does not match path ", show path, "."])

-- | A variatnt of putInPath that leaves the EJsonValue unchanged if the update is not sensible
--
--   Example:
--
--   >>> :set -XOverloadedStrings
--   >>> putInPath' ["a"] "b" "hello"
--   "hello"
--
putInPath' :: [Text] -> EJsonValue -> EJsonValue -> EJsonValue
putInPath' path payload target = case putInPath path payload target
                                   of Right x -> x
                                      Left  _ -> target

-- | modifyInPath modifies values in an EJsonValue object at a point indicated by a path.
--
--   Examples:
--
--   >>> :set -XOverloadedStrings
--
--   >>> modifyInPath [] (ejobject [("q","r")]) (ejobject [("x","y")])
--   Right {"q":"r","x":"y"}
--
--   If you attempt to update a value as if it were an EJObject when in-fact it is something else,
--   then you will receive an Error.
--
--   Example:
--
--   >>> modifyInPath ["a", "b"] "c" (ejobject [("a","hello")])
--   Left "Path [\"a\",\"b\"] not present in object {\"a\":\"hello\"}"
--
--   >>> modifyInPath ["a", "b"] (ejobject [("a","hello")]) "c"
--   Left "Path [\"a\",\"b\"] not present in object \"c\""
--
modifyInPath :: [Text] -> EJsonValue -> EJsonValue -> Either String EJsonValue
modifyInPath path modifications target =
  case Just target & pathTo path <<%~ fmap (simpleMerge modifications)
    of (Just _, Just r) -> Right r
       _                -> Left (concat ["Path ", show path, " not present in object ", show target])

simpleMerge :: EJsonValue -> EJsonValue -> EJsonValue
simpleMerge modifications = execState $ traverseOf_ _EJObject (mapM_ setPair . HM.toList) modifications
  where
  setPair (k,v) = _EJObjectKey k .= Just v

-- | A variatnt of modifyInPath that leaves the EJsonValue unchanged if the update is not sensible.
--
--   Example:
--
--   >>> :set -XOverloadedStrings
--
--   >>> modifyInPath' ["a", "b"] "c" (ejobject [("a","hello")])
--   {"a":"hello"}
--
--   >>> modifyInPath' ["a", "b"] (ejobject [("a","hello")]) "c"
--   "c"
--
modifyInPath' :: [Text] -> EJsonValue -> EJsonValue -> EJsonValue
modifyInPath' path modifications target =
  either (const target) id (modifyInPath path modifications target)

-- | removeFromPath removes values from an EJsonValue object at a point indicated by a path.
--
--   The path is a list of text values indicating successive object keys.
--
--   Examples:
--
--   >>> :set -XOverloadedStrings
--
--   >>> removeFromPath ["a"] (ejobject [("a","b"),("x","y")])
--   Right {"x":"y"}
--
--   If you attempt to update a value as if it were an EJObject when in-fact it is something else,
--   then you will receive an Error.
--
--   Example:
--
--   >>> removeFromPath ["a","q","r"] (ejobject [("x","y")])
--   Left "Path [\"a\",\"q\",\"r\"] not present in object {\"x\":\"y\"}"
--
removeFromPath :: [Text] -> EJsonValue -> Either String EJsonValue
removeFromPath path target = case (Just target & pathTo path <<.~ Nothing)
                               of (Just _, Just r) -> Right r
                                  _                -> Left (concat ["Path ", show path, " not present in object ", show target])

-- | Constructs a Traversal' along a path of EJObject keys.
--
--   Both ends of the traversal are maybes in order to allow self-composition,
--   and to allow the insertion/deletion of values at a point in the path.
--
pathTo :: [Text] -> Traversal' (Maybe EJsonValue) (Maybe EJsonValue)
pathTo path = foldl (.) id (map textToPrism path)

textToPrism :: Text -> Traversal' (Maybe EJsonValue) (Maybe EJsonValue)
textToPrism x = _Just . _EJObject . at x

-- | A variatnt of removeFromPath that leaves the EJsonValue unchanged if the update is not sensible
--
--   Example:
--
--   >>> :set -XOverloadedStrings
--
--   >>> removeFromPath' ["a","b"] (ejobject [("a", ejobject [("b","c")]), ("d","e")])
--   {"a":{},"d":"e"}
--
--   >>> removeFromPath' ["a"] "hello"
--   "hello"
--
removeFromPath' :: [Text] -> EJsonValue -> EJsonValue
removeFromPath' p v = either (const v) id (removeFromPath p v)

-- | Takes a path and an EJsonValue and wraps the EJsonValue in
--   successive EJObjects for each item in the path.
--
expand :: [Text] -> EJsonValue -> EJsonValue
expand path payload = foldr f payload path where f x y = ejobject [(x,y)]

-- | Construct a simple message object with no data.
--
makeMsg :: Text -> EJsonValue
makeMsg key = ejobject [("msg", ejstring key)]

-- | Construct a simple object with only an ID.
--
makeId :: Text -> EJsonValue
makeId key = ejobject [("id", ejstring key)]

-- | Construct a matcher for subscription-ready based on ID.
--
-- TODO: Allow for propper matcher behavior and abstraction a-la clojure's midje methods.
--       This is important as there could be multiple subscription ids listed here...
--
makeSubReady :: Text -> EJsonValue
makeSubReady key = ejobject [("msg","ready"), ("subs", ejarray [ejstring key])]

-- | Construct a matcher for subscription failure based on ID.
--
makeNoSub :: Text -> EJsonValue
makeNoSub key = ejobject [("msg","nosub"), ("id", ejstring key)]

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
