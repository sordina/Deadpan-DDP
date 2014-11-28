{-|
  Description: Making EJsonValue Control.Lens compatible through `Control.Lens.Prism`s

  Making EJsonValue Control.Lens compatible through `Control.Lens.Prism`s

  Since EJsonValue is a sum-type, you need to take advantage of the `Control.Lens.Prism`
  class provided by the Lens library if you wish to use it in a lens-compatible way.

  The set of instances is so-far incomplete.
-}

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.EJson.Prism
  ( _EJObject
  , _EJString
  )
  where

-- External Imports

import Data.Text
import Control.Lens
import Data.HashMap.Strict

-- Internal Imports

import Data.EJson.EJson


-- | _EJObject is a prism that allows access to the value represented by a
--   lookup via a key in to an EJObject.
--
--   This is constructed as a convenience so that you do not need to compose,
--   or even have knowledge of the underlying HashMap implementaiton of
--   EJObject.
--
_EJObject :: Text -> Prism' EJsonValue EJsonValue
_EJObject k = prism' (const EJNull) $ f -- TODO: Does const violate prism laws?
  where f (EJObject h) = Data.HashMap.Strict.lookup k h
        f _            = Nothing

prop_ejopristest_null :: Bool
prop_ejopristest_null = EJNull ^? _EJObject "key" == Nothing

prop_ejopristest_object :: Bool
prop_ejopristest_object = ejobject [("hello","world")] ^? _EJObject "hello" == Just "world"

-- | _EJString is a prism that points to the EJString constructor of
--   the EJsonValue data-type.
--
_EJString :: Prism' EJsonValue Text
_EJString = prism' (const EJNull) $ f -- TODO: Does const violate prism laws?
  where f (EJString s) = Just s
        f _            = Nothing

prop_ejspristest_string :: Bool
prop_ejspristest_string = ejstring "hello" ^? _EJString == Just "hello"
