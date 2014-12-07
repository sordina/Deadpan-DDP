{-|

  Description : Properties for the EJsonValue datatype

  Properties for the EJsonValue datatype.
-}

{-# LANGUAGE     OverloadedStrings #-}
{-# LANGUAGE     RankNTypes #-}
{-# LANGUAGE     TemplateHaskell #-}
{-# LANGUAGE     OverloadedStrings #-}
{-# LANGUAGE     TypeSynonymInstances #-}
{-# LANGUAGE     MultiParamTypeClasses #-}

module Data.EJson.Props where

import Data.EJson
import Control.Lens

prop_ejopristest_null :: Bool
prop_ejopristest_null = EJNull ^. _EJObjectKey "key" == Nothing

prop_ejopristest_object :: Bool
prop_ejopristest_object = ejobject [("hello","world")]
                       ^. _EJObjectKey "hello"
                       == Just "world"

prop_ejopristest_object2 :: Bool
prop_ejopristest_object2 = ejobject [("hello","world")]
                        ^? _EJObjectKey "hello"
                         . _Just
                         . _EJString
                        == Just "world"

prop_ejarrayixtest_null :: Bool
prop_ejarrayixtest_null = ejnull ^? _EJAraryIndex 1 == Nothing

prop_ejarrayixtest_array :: Bool
prop_ejarrayixtest_array = ejarray ["hello"]
                        ^? _EJAraryIndex 0
                        == Just "hello"
