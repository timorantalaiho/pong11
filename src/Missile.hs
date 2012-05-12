{-# LANGUAGE DeriveDataTypeable #-}
module Missile where

import Data.Data
import Data.Typeable

import Coordinate

type Missile = String
type Missiles = [String]
data Speed = Speed { x :: Float, y :: Float } deriving (Data, Typeable, Show)
data MissileLaunched = MissileLaunched { code :: String, speed :: Speed, pos :: Coordinates} deriving (Data, Typeable, Show)

