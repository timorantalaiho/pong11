{-# LANGUAGE DeriveDataTypeable #-}
module Domain where

import Data.Data
import Data.Typeable
import Coordinate

type BoardHistory = [Board]
type CommandHistory = [Command]
data Command = Command { timestamp :: Int, lastDirection :: Float } deriving (Data, Typeable, Show)
data State = State { boardHistory :: BoardHistory, commandHistory :: CommandHistory } deriving (Data, Typeable, Show)

emptyState :: State
emptyState = State emptyBoardHistory emptyCommandHistory

emptyCommandHistory :: CommandHistory
emptyCommandHistory = take 10 $ repeat (Command 0 0)

emptyBoardHistory :: BoardHistory
emptyBoardHistory = take 10 $ repeat emptyBoard

emptyBoard = Board 0 (Paddle 0 "") (Paddle 0 "") (Ball (Coordinates 30 40)) (Conf 640 480  50 10 5 15)  

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