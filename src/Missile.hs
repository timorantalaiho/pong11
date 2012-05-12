{-# LANGUAGE DeriveDataTypeable #-}
module Missile where

import Data.Data
import Data.Typeable

import Coordinate

type Missile = String
type Missiles = [String]
data Speed = Speed { x :: Float, y :: Float } deriving (Data, Typeable, Show)
data MissileLaunched = MissileLaunched { code :: String, speed :: Speed, pos :: Coordinates, launchTime :: Int } deriving (Data, Typeable, Show)

removeMissedMissiles :: Int -> Float -> [MissileLaunched] -> [MissileLaunched]
removeMissedMissiles currentTime maxWidth missiles = filter (inBoard currentTime maxWidth) missiles

inBoard :: Int -> Float -> MissileLaunched -> Bool
inBoard currentTime maxWidth missile = do
  let currentX = missileCurrentX currentTime missile
  (currentX > 0) && (currentX < maxWidth)
    
missileStartX :: MissileLaunched -> Float
missileStartX m = Coordinate.x $ Missile.pos $ m

missileCurrentX :: Int -> MissileLaunched -> Float
missileCurrentX currentTime missile = startX + (runtime * missileSpeed) 
  where startX = missileStartX missile
        runtime = fromIntegral $ currentTime - (launchTime $ missile)
        missileSpeed = Missile.x $ speed $ missile
  
