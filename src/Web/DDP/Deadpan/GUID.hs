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


module Web.DDP.Deadpan.GUID ( module Web.DDP.Deadpan.GUID) where

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


hashTriple :: IO (Integer,UTCTime,Integer)
hashTriple = do
  cpu  <- getCPUTime
  time <- getCurrentTime
  rand <- randomIO
  return (cpu,time,rand)

newGuid :: IO Int
newGuid = hash `fmap` hashTriple

newGuidString :: IO String
newGuidString = (show . hash) `fmap` hashTriple

newGuidText :: IO Text
newGuidText = pack `fmap` newGuidString
