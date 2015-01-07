{-|

  Description : Properties for the EJsonValue datatype

  Properties for the EJsonValue datatype.
-}

{-# LANGUAGE     RankNTypes #-}
{-# LANGUAGE     OverloadedStrings #-}
{-# LANGUAGE     TypeSynonymInstances #-}
{-# LANGUAGE     MultiParamTypeClasses #-}

module Data.EJson.Props where

import Data.EJson
import Data.EJson.Aeson ()

import Data.Aeson
import qualified Data.ByteString.Lazy as BS

-- | prop> prop_ejopristest_null
prop_ejopristest_null :: Bool
prop_ejopristest_null = EJNull ^. _EJObjectKey "key" == Nothing

-- | prop> prop_ejopristest_object
prop_ejopristest_object :: Bool
prop_ejopristest_object = ejobject [("hello","world")]
                       ^. _EJObjectKey "hello"
                       == Just "world"

-- | prop> prop_ejopristest_object2
prop_ejopristest_object2 :: Bool
prop_ejopristest_object2 = ejobject [("hello","world")]
                        ^? _EJObjectKey "hello"
                         . _Just
                         . _EJString
                        == Just "world"

-- | prop> prop_ejarrayixtest_null
prop_ejarrayixtest_null :: Bool
prop_ejarrayixtest_null = ejnull ^? _EJAraryIndex 1 == Nothing

-- | prop> prop_ejarrayixtest_array
prop_ejarrayixtest_array :: Bool
prop_ejarrayixtest_array = ejarray ["hello"]
                        ^? _EJAraryIndex 0
                        == Just "hello"

-- Examples from real apps

decodeEJ :: BS.ByteString -> Maybe EJsonValue
decodeEJ = decode

ttt :: (a -> Bool) -> Maybe a -> Bool
ttt f v = Just True == ffv && length (show ffv) > 0 where ffv = fmap f v

-- | prop> prop_decodable_1
prop_decodable_1 :: Bool
prop_decodable_1 = ttt isEJDate $ decodeEJ "{\"$date\":1418373430495}"

-- | prop> prop_decodable_2
prop_decodable_2 :: Bool
prop_decodable_2 = ttt isEJObject $ decodeEJ "{\"msg\":\"added\",\"collection\":\"todos\",\"id\":\"jsoQsi4QWhTjyLM3v\",\"fields\":{\"checked\":false,\"createdAt\":{\"$date\":1418373430495},\"listId\":\"By8CtgWGvbZfJPFsd\",\"text\":\"Data on the Wire\"}}"

-- | prop> prop_decodable_3
prop_decodable_3 :: Bool
prop_decodable_3 = ttt isEJObject $ decodeEJ "{\"msg\":\"ready\",\"subs\":[\"42463a77-578f-4503-8d2b-637a3c6c9ed6\"]}"

-- | prop> prop_decodable_4
prop_decodable_4 :: Bool
prop_decodable_4 = ttt isEJObject $ decodeEJ "{\"msg\":\"added\",\"collection\":\"lists\",\"id\":\"By8CtgWGvbZfJPFsd\",\"fields\":{\"name\":\"Meteor Principles\",\"incompleteCount\":6}}"

-- | prop> prop_decodable_5
prop_decodable_5 :: Bool
prop_decodable_5 = ttt isEJObject $ decodeEJ "{\"msg\":\"added\",\"collection\":\"lists\",\"id\":\"sqrAXjWmzAE6WZhdf\",\"fields\":{\"name\":\"Languages\",\"incompleteCount\":6}}"

-- | prop> prop_decodable_6
prop_decodable_6 :: Bool
prop_decodable_6 = ttt isEJObject $ decodeEJ "{\"msg\":\"added\",\"collection\":\"lists\",\"id\":\"F73xFyAuKrqsb2J3m\",\"fields\":{\"name\":\"Favorite Scientists\",\"incompleteCount\":6}}"

-- | prop> prop_decodable_7
prop_decodable_7 :: Bool
prop_decodable_7 = ttt isEJObject $ decodeEJ "{\"msg\":\"added\",\"collection\":\"lists\",\"id\":\"pY53869jRS5LdLuze\",\"fields\":{\"incompleteCount\":0,\"name\":\"List foo\"}}"

-- | prop> prop_decodable_8
prop_decodable_8 :: Bool
prop_decodable_8 = ttt isEJObject $ decodeEJ "{\"msg\":\"ready\",\"subs\":[\"8a82270b-8087-4dcc-90d3-ae758d236056\"]}"

-- | prop> prop_decodable_9
prop_decodable_9 :: Bool
prop_decodable_9 = ttt isEJObject $ decodeEJ "{\"msg\":\"added\",\"collection\":\"todos\",\"id\":\"wf2dBLkR78vebkPMf\",\"fields\":{\"checked\":true,\"createdAt\":{\"$date\":1418373430499},\"listId\":\"By8CtgWGvbZfJPFsd\",\"text\":\"Full Stack Reactivity\"}}"
