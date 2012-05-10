{-# LANGUAGE DeriveDataTypeable #-}
module Domain where

import Data.Data
import Data.Typeable
import Coordinate

type State = [Board]
data Paddle = Paddle { y :: Float, playerName :: String } deriving (Data, Typeable, Show)
data Ball = Ball { pos :: Coordinates } deriving (Data, Typeable, Show)
data Conf = Conf { maxWidth :: Int, maxHeight :: Int, paddleHeight :: Int, paddleWidth :: Int, ballRadius :: Int, tickInterval :: Int} deriving (Data, Typeable, Show)
data Board = Board { time :: Int, left :: Paddle, right :: Paddle, ball :: Ball, conf :: Conf } deriving (Data, Typeable, Show)
type Velocity = Coordinates

paddleMiddleY :: Board -> Float
paddleMiddleY board =
  paddleY + ((fromIntegral $ paddleHeight $ conf board) / 2.0)
  where paddleY = Domain.y $ left board

extractBallCoordinates :: Board -> Coordinates
extractBallCoordinates board = pos $ ball board

