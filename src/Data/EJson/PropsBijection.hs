
{-|

  Description : Bijection Properties for the EJsonValue datatype

  Bijection Properties for the EJsonValue datatype
-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE     RankNTypes #-}
{-# LANGUAGE     FlexibleInstances #-}
{-# LANGUAGE     OverloadedStrings #-}
{-# LANGUAGE     TypeSynonymInstances #-}
{-# LANGUAGE     MultiParamTypeClasses #-}

-- https://github.com/jb55/aeson-util/blob/master/Data/Aeson/Util/Test.hs
-- Supplied inspiration for Arbitrary instances
--
-- TODO: Clean this up by using a proper library for Value Arbitrary instance

module Data.EJson.Props where

import Data.EJson
import Data.Maybe
import Data.Aeson as A
import Data.Text as T
import Test.QuickCheck
import Control.Applicative
import Data.Scientific
import Data.Hashable (Hashable)
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V

instance Arbitrary Text where
    arbitrary = T.pack <$> (arbitrary :: Gen String)

instance Arbitrary A.Array where
  arbitrary = V.fromList <$> vectorOf 2 (arbitrary :: Gen A.Value)

instance (Eq a, Hashable a, Arbitrary a, Arbitrary b) => Arbitrary (HM.HashMap a b) where
  arbitrary = HM.fromList <$> vectorOf 2 (arbitrary :: (Arbitrary a, Arbitrary b) => Gen (a, b))

instance Arbitrary Scientific where
    arbitrary = fromIntegral <$> (arbitrary :: Gen Int)

instance Arbitrary A.Value where
  arbitrary = oneof [
        A.String <$> (arbitrary :: Gen Text)
      , A.Array  <$> (arbitrary :: Gen Array)
      , A.Object <$> (arbitrary :: Gen Object)
      , A.Number <$> (arbitrary :: Gen Scientific)
      , A.Bool   <$> (arbitrary :: Gen Bool)
      , return A.Null
    ]

  shrink (A.Array a)  = fromMaybe [] (a V.!? 0 >>= return . (:[]))
  shrink (A.Object _) = []
  shrink _            = []

-- | prop> prop_value2EJson_bijection
prop_value2EJson_bijection :: Value -> Bool
prop_value2EJson_bijection val = val == ejson2value (value2EJson val)

-- TODO: Go back the other way...
