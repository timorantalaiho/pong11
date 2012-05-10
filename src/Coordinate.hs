{-# LANGUAGE DeriveDataTypeable #-}
module Coordinate where

import Data.Data
import Data.Typeable

data Coordinates = Coordinates { x :: Float, y :: Float } deriving (Data, Typeable, Show)
