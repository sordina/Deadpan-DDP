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

  The internals of EJson are defined in `Data.EJson.EJson`.

  A Prism' instance is defined in `Data.EJson.Prism`.
-}

module Data.EJson (

    module Data.EJson.EJson
  , module Data.EJson.Prism

  ) where

import Data.EJson.EJson
import Data.EJson.Prism

