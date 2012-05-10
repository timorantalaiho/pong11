{-# LANGUAGE DeriveDataTypeable #-}
module Domain where

import Data.Data
import Data.Typeable

data ChessBoard = ChessBoard { pieces :: [Piece]} deriving (Data, Typeable, Show)
data Piece = Piece { color :: String, position :: String} deriving (Data, Typeable, Show)


data Paddle = Paddle { y :: Int, playerName :: String } deriving (Data, Typeable, Show)
data Pos = Pos { posX :: Int, posY :: Int } deriving (Data, Typeable, Show)
data Conf = Conf { maxWidth :: Int, maxHeight :: Int, paddleHeight :: Int, paddleWidth :: Int, ballRadius :: Int, tickInterval :: Int} deriving (Data, Typeable, Show)
data Board = Board { time :: Int, left :: Paddle, right :: Paddle, ball :: Pos, conf :: Conf } deriving (Data, Typeable, Show)