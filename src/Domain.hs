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

paddleMiddleY :: Board -> Float -> Float
paddleMiddleY board paddleY = paddleY + (height / 2.0)
  where height = fromIntegral $ paddleHeight $ conf $ board

leftPaddleMiddleY :: Board -> Float
leftPaddleMiddleY board = paddleMiddleY board leftY  
  where leftY = Domain.y $ left board

rightPaddleMiddleY :: Board -> Float
rightPaddleMiddleY board = paddleMiddleY board rightY  
  where rightY = Domain.y $ right board

paddleW :: Board -> Float
paddleW board = fromIntegral $ paddleWidth $ conf board

paddleH :: Board -> Float
paddleH board = fromIntegral $ paddleHeight $ conf board

leftWallX :: Board -> Float
leftWallX = paddleW

rightWallX :: Board -> Float
rightWallX board = (boardWidth board) - (paddleW board)

boardWidth :: Board -> Float
boardWidth board = fromIntegral $ maxWidth $ conf board

boardHeight :: Board -> Float
boardHeight board = fromIntegral $ maxHeight $ conf board

extractBallCoordinates :: Board -> Coordinates
extractBallCoordinates board = pos $ ball board

ballR :: Board -> Int 
ballR board = ballRadius $ conf $ board 