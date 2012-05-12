{-# LANGUAGE DeriveDataTypeable #-}
module Domain where

import Data.Data
import Data.Typeable
import Coordinate
import Missile

type BoardHistory = [Board]
type CommandHistory = [Command]
data Command = Command { timestamp :: Int, lastDirection :: Float } deriving (Data, Typeable, Show)

data State = State { boardHistory :: BoardHistory, commandHistory :: CommandHistory, missiles :: Missiles, launchedMissiles :: [MissileLaunched] } deriving (Data, Typeable, Show)

emptyState :: State
emptyState = State emptyBoardHistory emptyCommandHistory [] []

emptyCommandHistory :: CommandHistory
emptyCommandHistory = take 100 $ repeat (Command 0 0)

emptyBoardHistory :: BoardHistory
emptyBoardHistory = take 10 $ repeat emptyBoard

emptyBoard = Board 0 (Paddle 0 "") (Paddle 0 "") (Ball (Coordinates 30 40)) (Conf 640 480  50 10 5 15)  

data Paddle = Paddle { y :: Float, playerName :: String } deriving (Data, Typeable, Show)
data Ball = Ball { pos :: Coordinates } deriving (Data, Typeable, Show)
data Conf = Conf { maxWidth :: Int, maxHeight :: Int, paddleHeight :: Int, paddleWidth :: Int, ballRadius :: Int, tickInterval :: Int} deriving (Data, Typeable, Show)
data Board = Board { time :: Int, left :: Paddle, right :: Paddle, ball :: Ball, conf :: Conf } deriving (Data, Typeable, Show)
type Velocity = Coordinates

paddleMiddleY :: Board -> Float -> Float
paddleMiddleY board paddleY = paddleY + ((paddleH board) / 2.0)

leftPaddleMiddleY :: Board -> Float
leftPaddleMiddleY board = paddleMiddleY board leftY  
  where leftY = Domain.y $ left board

rightPaddleMiddleY :: Board -> Float
rightPaddleMiddleY board = paddleMiddleY board rightY  
  where rightY = Domain.y $ right board

paddleW :: Board -> Float
paddleW = fromIntegral . paddleWidth . conf

paddleH :: Board -> Float
paddleH = fromIntegral . paddleHeight . conf

leftWallX :: Board -> Float
leftWallX = paddleW

rightWallX :: Board -> Float
rightWallX board = (boardWidth board) - (paddleW board)

boardWidth :: Board -> Float
boardWidth = fromIntegral . maxWidth . conf

boardHeight :: Board -> Float
boardHeight = fromIntegral . maxHeight . conf

extractBallCoordinates :: Board -> Coordinates
extractBallCoordinates = Domain.pos . ball

ballR :: Board -> Int 
ballR = ballRadius . conf