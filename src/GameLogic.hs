module GameLogic where

import Domain
import Coordinate
import Debug.Trace

calculateDirection :: State -> Float
calculateDirection state =
  chooseDirection current target
  where board = head state
        current = leftPaddleMiddleY board
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

vectorFrom :: Coordinates -> Coordinates -> Coordinates
vectorFrom c1 c2 = Coordinates ((Coordinate.x c1) + (Coordinate.x c2)) ((Coordinate.y c1) + (Coordinate.y c2))

chooseDirection :: Float -> Float -> Float
chooseDirection currentY targetY
  | difference < 0.0 = -1.0
  | difference > 0.0 = 1.0
  | otherwise = 0.0
  where difference = targetY - currentY

targetY :: State -> Float
targetY state =
    Coordinate.y $ traceBallToOurPaddle p v b
  where b = head state
        p = extractBallCoordinates b
        v = ballVelocity state

traceBallToOurPaddle :: Coordinates -> Velocity -> Board -> Coordinates
traceBallToOurPaddle p v board
    | ballStopped v = p
    | fst ourPaddleHit = p'
    | otherwise = traceBallToOurPaddle p' v' board
    where p' = advanceBall p v
          possibleHits = [ourPaddleHit, opponentPaddleHit, ceilingHit, floorHit]
          wouldHit = foldl (||) False (map fst possibleHits)
          ourPaddleHit = hitsOurPaddle p' v board
          opponentPaddleHit = hitsOpponentPaddle p' v board
          ceilingHit = hitsCeiling p' v board
          floorHit = hitsFloor p' v board
          v'
           | wouldHit = snd $ head $ filter fst possibleHits
           | otherwise = v

advanceBall :: Coordinates -> Velocity -> Coordinates
advanceBall p v =
    vectorFrom p v

ballStopped :: Velocity -> Bool
ballStopped v = ((Coordinate.y v) == 0.0) || ((Coordinate.x v) == 0.0)

hitsOurPaddle :: Coordinates -> Velocity -> Board -> (Bool, Velocity)
hitsOurPaddle hit v board = (isHit, v')
    where isHit = (x hit) <= (leftWallX board)
          v'
            | isHit = deflectFromPaddle v
            | otherwise = v

hitsOpponentPaddle :: Coordinates -> Velocity -> Board -> (Bool, Velocity)
hitsOpponentPaddle hit v board = (isHit, v')
    where isHit = (x hit) >= (rightWallX board)
          v'
            | isHit = deflectFromPaddle v
            | otherwise = v

hitsCeiling :: Coordinates -> Velocity -> Board -> (Bool, Velocity)
hitsCeiling hit v board = (isHit, v')
    where isHit = (Coordinate.y hit) <= 0.0
          v'
            | isHit = deflectFromWall v
            | otherwise = v

hitsFloor :: Coordinates -> Velocity -> Board -> (Bool, Velocity)
hitsFloor hit v board = (isHit, v')
    where isHit = (Coordinate.y hit) >= (boardHeight board)
          v'
            | isHit = deflectFromWall v
            | otherwise = v

deflectFromPaddle :: Velocity -> Velocity
deflectFromPaddle v = Coordinates (-Coordinate.x v) (Coordinate.y v)

deflectFromWall :: Velocity -> Velocity
deflectFromWall v = Coordinates (Coordinate.x v) (-Coordinate.y v)
        
-- Test data
start_v2 = Coordinates 1 1
start_p = Coordinates 15.0 15.0
board = Board 123456 (Paddle 19 "foo") (Paddle 14 "bar") (Ball (Coordinates 30 40)) (Conf 640 480  50 10 5 15)
