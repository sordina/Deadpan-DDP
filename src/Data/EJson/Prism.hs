{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.EJson.Prism where

-- External Imports

import Data.Text
import Control.Lens
import Data.HashMap.Strict

-- Internal Imports

import Data.EJson.EJson


-- TODO: Remove this "helpful" documentation

-- prism :: (b -> t) -> (s -> Either t a) -> Prism s t a b
--
-- type Prism s t a b =
-- forall (p :: * -> * -> *) (f :: * -> *).
-- (Choice p, Control.Applicative.Applicative f) =>
-- p a (f b) -> p s (f t)
--
-- prism' :: (b -> s) -> (s -> Maybe a) -> Prism s s a b
--
-- preview ::
-- Control.Monad.Reader.Class.MonadReader s m =>
-- Getting (Data.Monoid.First a) s a -> m (Maybe a)
--
-- _Just :: Prism (Maybe a) (Maybe b) a b
-- _Just = prism Just $ maybe (Left Nothing) Right
--
-- _Left :: Prism (Either a c) (Either b c) a b
-- _Left = prism Left $ either Right (Left . Right)
--
-- _Right :: Prism (Either c a) (Either c b) a b
-- _Right = prism Right $ either (Left . Left) Right
--
-- _EJObject :: Text -> Prism EJsonValue (Maybe EJsonValue) EJsonValue EJsonValue

_EJObject :: Text -> Prism' EJsonValue EJsonValue
_EJObject k = prism' (const EJNull) $ f -- TODO: Does const violate prism laws?
  where f (EJObject h) = Data.HashMap.Strict.lookup k h
        f _            = Nothing

prop_ejopristest_null :: Bool
prop_ejopristest_null = EJNull ^? _EJObject "key" == Nothing

prop_ejopristest_object :: Bool
prop_ejopristest_object = ejobject [("hello","world")] ^? _EJObject "hello" == Just "world"
