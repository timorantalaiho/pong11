module GameLogic where

import Domain
import Coordinate

calculateDirection :: State -> Float
calculateDirection state =
  chooseDirection current target
  where board = head state
        current = paddleMiddleY board
        target = targetY state

ballVelocity :: State -> Velocity
ballVelocity [] = Coordinates 0.0 0.0
ballVelocity [x] = Coordinates 0.0 0.0
ballVelocity (s1:s2:xs) =
  vectorTo newCoordinates oldCoordinates
  where newCoordinates = extractBallCoordinates s1
        oldCoordinates = extractBallCoordinates s2

vectorTo :: Coordinates -> Coordinates -> Coordinates
vectorTo c1 c2 = Coordinates ((Coordinate.x c1) - (Coordinate.x c2)) ((Coordinate.y c1) - (Coordinate.y c2))

chooseDirection :: Float -> Float -> Float
chooseDirection currentY targetY
  | difference < 0.0 = -1.0
  | difference > 0.0 = 1.0
  | otherwise = 0.0
  where difference = targetY - currentY

targetY :: State -> Float
--targetY state = Coordinate.y $ Domain.pos $ Domain.ball board
--  where board = head state  
targetY state = Coordinate.y leftHit
  where hits = nextHits state
        hitsPaddle hit = (x hit) == 0
        leftHit = head $ filter hitsPaddle hits
        
hitsPaddle :: Coordinates -> Board -> Bool
hitsPaddle hit board = (x hit) == (leftWallX board)

hitsOtherPaddle :: Coordinates -> Board -> Bool
hitsOtherPaddle hit board = (x hit) >= ((fromIntegral $ maxWidth $ conf board) - (fromIntegral $ paddleWidth $ conf board))

hitsCeiling :: Coordinates -> Board -> Bool
hitsCeiling hit board = (Coordinate.y hit) <= 0.0

hitsFloor :: Coordinates -> Board -> Bool
hitsFloor hit board = (Coordinate.y hit) >= (fromIntegral $ maxHeight $ conf board)
        
nextHits :: State -> [Coordinates]
nextHits state = [Coordinates 0 0]

