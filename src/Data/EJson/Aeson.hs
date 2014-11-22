
{-|

  Description : Aeson instances for EJsonValue

  Piggybacks off the `Data.Aeson.Value` type.

-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE     OverloadedStrings #-}

module Data.EJson.Aeson where

import Data.Aeson
import Data.EJson

-- TODO: Consider implementing these translations directly, rather than passing through 'Value'.
--
instance FromJSON EJsonValue where parseJSON = return . value2EJson
instance ToJSON   EJsonValue where toJSON    = ejson2value
