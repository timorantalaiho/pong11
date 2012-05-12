{-# LANGUAGE DeriveDataTypeable #-}
module MissileLogic where

import Network
import System.IO(Handle, hFlush, hPutChar)
import Data.Data
import Data.Typeable

import Coordinate
import Domain
import Missile
import Json

sendmissile :: Handle -> Board -> Velocity -> [Coordinates] -> Missiles -> IO( Missiles)
sendmissile h board v [] [] = return ([])
sendmissile h board v  _ [] = return ([])
sendmissile h board v []  _ = return ([])
sendmissile h board v wayPoints (x:xs) = do
  let ly = (leftPaddleMiddleY board)
      ry = (rightPaddleMiddleY board)
      --ballGoingAway = Coordinate.x v > 0
      rightHit = filter (\w -> abs(ly - (Coordinate.x w)) < 10) wayPoints
      enemyDir = enemyDirection ry rightHit
      --hasRightHit = length rightHit > 0
      --launch = ballGoingAway && hasRightHit && abs(ly - (Coordinate.y $ head rightHit)) < 50
      --launch = ballGoingAway && abs(ly -ry) < 50
      --launch = abs(ly - ry) < 10
      launch = isCloseEnough ly ry enemyDir
  case launch of 
    True -> do 
      send h "launchMissile" x
      return (xs)
    False -> do 
      return (x:xs)

enemyDirection :: Float -> [Coordinates] -> Float
enemyDirection curr [] = 0.0
enemyDirection curr (next:xs)
  | curr - Coordinate.y next > 0 = -(1.0)
  | otherwise = 1.0

isCloseEnough :: Float -> Float -> Float -> Bool
isCloseEnough ly ry enemyDirection
  | enemyDirection == 0.0 = abs(ly - ry) < 10
  | enemyDirection > 0.0 = (ly - ry > 10) && (ly - ry < 50)
  | enemyDirection < 0.0 = (ly - ry < 10) && (ly - ry > -50)
  