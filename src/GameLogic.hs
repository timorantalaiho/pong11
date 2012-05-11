module GameLogic where

import Domain
import Coordinate
import Data.Maybe
import Debug.Trace

calculateDirection :: BoardHistory -> Float
calculateDirection history =
  chooseDirection current target board
  where board = head history
        current = leftPaddleMiddleY board
        target = targetY history

ballVelocity :: BoardHistory -> Velocity
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

chooseDirection :: Float -> Float -> Board -> Float
chooseDirection currentY targetY board
  | difference < negHalfPaddleH = -1.0
  | difference > posHalfPaddleH =  1.0
  | otherwise = 0.0
  where difference = targetY - currentY
        posHalfPaddleH = (paddleH board) / 2.0
        negHalfPaddleH = negate posHalfPaddleH

targetY :: BoardHistory -> Float
targetY (x:xs) =
    Coordinate.y $ traceBallToOurPaddle p v b
  where b = x
        p = extractBallCoordinates b
        v = ballVelocity (x:xs)
targetY [] = 0.0


traceBallToOurPaddle :: Coordinates -> Velocity -> Board -> Coordinates
traceBallToOurPaddle p v board
    | ballStopped v = p
    | isJust ourPaddleHit = p'
    | otherwise = traceBallToOurPaddle p' v' board
    where p' = advanceBall p v
          possibleHits = [ourPaddleHit, opponentPaddleHit, ceilingHit, floorHit]
          hit = filter isJust possibleHits
          ourPaddleHit = hitsOurPaddle p' v board
          opponentPaddleHit = hitsOpponentPaddle p' v board
          ceilingHit = hitsCeiling p' v board
          floorHit = hitsFloor p' v board
          v' = case hit of
              [Just(x)] -> x
              _ -> v

advanceBall :: Coordinates -> Velocity -> Coordinates
advanceBall p v =
    vectorFrom p v

ballStopped :: Velocity -> Bool
ballStopped v = ((Coordinate.y v) == 0.0) || ((Coordinate.x v) == 0.0)

hitsOurPaddle :: Coordinates -> Velocity -> Board -> Maybe Velocity
hitsOurPaddle hit v board = v'
    where isHit = (x hit) <= (leftWallX board)
          v'
            | isHit = Just $ deflectFromPaddle v
            | otherwise = Nothing

hitsOpponentPaddle :: Coordinates -> Velocity -> Board -> Maybe Velocity
hitsOpponentPaddle hit v board = v'
    where isHit = (x hit) >= (rightWallX board)
          v'
            | isHit = Just $ deflectFromPaddle v
            | otherwise = Nothing

hitsCeiling :: Coordinates -> Velocity -> Board -> Maybe Velocity
hitsCeiling hit v board = v'
    where isHit = (Coordinate.y hit) <= 0.0
          v'
            | isHit = Just $ deflectFromWall v
            | otherwise = Nothing

hitsFloor :: Coordinates -> Velocity -> Board -> Maybe Velocity
hitsFloor hit v board = v'
    where isHit = (Coordinate.y hit) >= (boardHeight board)
          v'
            | isHit = Just $ deflectFromWall v
            | otherwise = Nothing

deflectFromPaddle :: Velocity -> Velocity
deflectFromPaddle v = Coordinates (-Coordinate.x v) (Coordinate.y v)

deflectFromWall :: Velocity -> Velocity
deflectFromWall v = Coordinates (Coordinate.x v) (-Coordinate.y v)
        
-- Test data
start_v2 = Coordinates 1 1
start_p = Coordinates 15.0 15.0
board = Board 123456 (Paddle 19 "foo") (Paddle 14 "bar") (Ball (Coordinates 30 40)) (Conf 640 480  50 10 5 15)
