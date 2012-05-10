{-# LANGUAGE DeriveDataTypeable #-}
module Coordinate where

import Data.Data
import Data.Typeable

data Coordinates = Coordinates { x :: Int, y :: Int } deriving (Data, Typeable, Show)
