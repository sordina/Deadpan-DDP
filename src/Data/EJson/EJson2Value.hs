
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-} -- I'm defining the orphan

module Data.EJson.EJson2Value ( ejson2value ) where

import Data.Aeson
import Data.Text.Internal
import Data.Text.Encoding
import Data.Vector
import Data.HashMap.Strict
import Data.ByteString.Base64
import Data.Time.Clock.POSIX
import Data.Time.Clock

-- Display purposes
import qualified Data.ByteString.Lazy.Char8 as BC8

import Data.EJson.EJson

instance Show EJsonValue
  where
  show = BC8.unpack . Data.Aeson.encode . ejson2value

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

makeUser :: Text -> EJsonValue -> Value
makeUser t v = Object
           $ Data.HashMap.Strict.fromList
           [ ("$type" , String t)
           , ("$value", ejson2value v)]

makeJsonDate :: UTCTime -> Value
makeJsonDate t = Object
               $ Data.HashMap.Strict.fromList
               [ ("$date", Number t') ]
  where
  t' = realToFrac $ utcTimeToPOSIXSeconds t
