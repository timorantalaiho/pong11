{-# LANGUAGE DeriveDataTypeable #-}
module LolAi where

import Data.Data
import Data.Typeable
import Coordinate
import Domain

-- Collision only with y = n line
data Edge = Top|Bottom|Left|Right deriving (Show, Eq)
data Collision = Collision { edge :: Edge, coordinates :: Coordinates } deriving (Show)

timeToBorder :: Float -> Float -> Float -> Maybe Float
timeToBorder ballY 0.0 maxY = Nothing
timeToBorder ballY velocityY maxY =
  Just ((maxY - ballY) / velocityY)
  
maybeCollisionPoint maxX minX xPos
  | xPos > minX && xPos < maxX = Just xPos
  | otherwise = Nothing
  
collisionPointWithBorder :: Float -> Float -> Float -> Float -> Maybe Float -> Maybe Float
collisionPointWithBorder ballX maxX minX xVelocity Nothing = Nothing
collisionPointWithBorder ballX maxX minX xVelocity (Just time) =
  maybeCollisionPoint maxX minX xPos
  where xPos = ballX + (xVelocity * time)
        
nextCollisionOnLeftEdge :: [Collision] -> Collision
nextCollisionOnLeftEdge = head . filter (\collision -> (LolAi.edge $ collision) == LolAi.Left)

targetY :: State -> Float
targetY state =
  Coordinate.y $ coordinates $ nextCollisionOnLeftEdge $ collisions
  where collisions = [Collision LolAi.Left (Coordinates 3.0 2.0)]
        
runTests = do
  let velY = -1.0
  let posY = 10.0
  putStrLn $ show $ collisionPointWithBorder 10.0 100.0 0.0 1.0 $ timeToBorder posY velY 0.0
  putStrLn $ show $ collisionPointWithBorder 10.0 15.0 0.0 1.0 $ timeToBorder posY velY 0.0
  

