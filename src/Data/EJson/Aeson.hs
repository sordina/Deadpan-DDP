
{-|

  Description : Aeson instances for EJsonValue

  Piggybacks off the `Data.Aeson.Value` type.

  This module provided for the convenience of users who wish to use the
  EJsonValue datatype in their own projects.
-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE     OverloadedStrings #-}

module Data.EJson.Aeson where

import Data.Aeson
import Data.EJson.EJson
import Data.EJson.EJson2Value

-- TODO: Consider implementing these translations directly, rather than passing through 'Value'.

-- | A FromJSON instance is provided for EJsonValue in order to be able to
--   take advantage of the Aeson functionality.
--
-- This is not used internally.
instance FromJSON EJsonValue where parseJSON = return . value2EJson

-- | A ToJSON instance is provided for EJsonValue in order to be able to
--   take advantage of the Aeson functionality.
--
-- This is not used internally.
instance ToJSON   EJsonValue where toJSON    = ejson2value
