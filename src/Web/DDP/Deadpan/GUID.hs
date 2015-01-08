{-|

Description: A small helper to generate GUIDs

A small helper to generate GUIDs.

Provides functions to generate simple GUIDs.

-}

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}


module Web.DDP.Deadpan.GUID ( GUID() -- No export of constructors in order to protect generation of IDs
                            , getGuidText
                            , newGuid
                            , hashTriple
                            , newGuidInt
                            , newGuidString
                            , newGuidText
                            , makeEJsonId
                            , ejson2guid
                            ) where

-- Internal Imports

import Data.EJson

-- External Imports

import System.Random
import System.CPUTime
import Data.Time
import Data.Text
import Data.Hashable
import GHC.Generics

instance Hashable DiffTime where
    hashWithSalt s = (hashWithSalt s :: Double -> Int) . realToFrac

deriving instance Generic Day
instance Hashable Day

deriving instance Generic UTCTime
instance Hashable UTCTime

newtype GUID = GUID {getGuidText :: Text} deriving (Eq,Ord,Generic,Hashable)

instance Show GUID where show = show . getGuidText


hashTriple :: IO (Integer,UTCTime,Integer)
hashTriple = do
  cpu  <- getCPUTime
  time <- getCurrentTime
  rand <- randomIO
  return (cpu,time,rand)

newGuidInt :: IO Int
newGuidInt = hash `fmap` hashTriple

newGuidString :: IO String
newGuidString = (show . hash) `fmap` hashTriple

newGuidText :: IO Text
newGuidText = pack `fmap` newGuidString

newGuid :: IO GUID
newGuid = GUID `fmap` newGuidText

makeEJsonId :: GUID -> EJsonValue
makeEJsonId key = ejobject [("id", ejstring (getGuidText key))]

ejson2guid :: EJsonValue -> Maybe GUID
ejson2guid v = fmap GUID $ v ^? _EJObjectKeyString "id"
