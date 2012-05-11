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
    where possibleHits = [ourPaddleHit, opponentPaddleHit, ceilingHit, floorHit]
          hit = filter isJust possibleHits
          ourPaddleHit = hitsOurPaddle p v board
          opponentPaddleHit = hitsOpponentPaddle p v board
          ceilingHit = hitsCeiling p v board
          floorHit = hitsFloor p v board
          (v',p') = case hit of
              [Just (nv,np)] -> (nv,np)
              _ -> (v,p)

ballStopped :: Velocity -> Bool
ballStopped v = ((Coordinate.y v) == 0.0) || ((Coordinate.x v) == 0.0)

hitsOurPaddle :: Coordinates -> Velocity -> Board -> Maybe (Velocity,Coordinates)
hitsOurPaddle p (Coordinates 0.0 y) board = Nothing
hitsOurPaddle p (Coordinates vx vy) board
    | timeToInpact >= 0.0 && yPos >= 0.0 && yPos <= (boardHeight board) = Just (v', p')
    | otherwise = Nothing
    where timeToInpact = ((leftWallX board) - (Coordinate.x p)) / vx
          yPos = (Coordinate.y p) + (vy * timeToInpact)
          p' = Coordinates (leftWallX board) yPos
          v' = deflectFromPaddle (Coordinates vx vy)

hitsOpponentPaddle :: Coordinates -> Velocity -> Board -> Maybe (Velocity,Coordinates)
hitsOpponentPaddle p (Coordinates 0.0 y) board = Nothing
hitsOpponentPaddle p (Coordinates vx vy) board
    | timeToInpact >= 0.0 && yPos >= 0.0 && yPos <= (boardHeight board) = Just (v', p')
    | otherwise = Nothing
    where timeToInpact = ((rightWallX board) - (Coordinate.x p)) / vx
          yPos = (Coordinate.y p) + (vy * timeToInpact)
          p' = Coordinates (rightWallX board) yPos
          v' = deflectFromPaddle (Coordinates vx vy)

hitsCeiling :: Coordinates -> Velocity -> Board -> Maybe (Velocity,Coordinates)
hitsCeiling p (Coordinates x 0.0) board = Nothing
hitsCeiling p (Coordinates vx vy) board
  | timeToInpact >= 0.0 && xPos >= (leftWallX board) && xPos <= (rightWallX board) = Just (v', p')
  | otherwise = Nothing
  where timeToInpact = (0.0 - (Coordinate.y p)) / vy
        xPos = (Coordinate.x p) + (vx * timeToInpact)
        p' = Coordinates xPos 0.0
        v' = deflectFromWall (Coordinates vx vy)

hitsFloor :: Coordinates -> Velocity -> Board -> Maybe (Velocity,Coordinates)
hitsFloor p (Coordinates x 0.0) board = Nothing
hitsFloor p (Coordinates vx vy) board
  | timeToInpact >= 0.0 && xPos >= (leftWallX board) && xPos <= (rightWallX board) = Just (v', p')
  | otherwise = Nothing
  where timeToInpact = ((boardHeight board) - (Coordinate.y p)) / vy
        xPos = (Coordinate.x p) + (vx * timeToInpact)
        p' = Coordinates xPos (boardHeight board)
        v' = deflectFromWall (Coordinates vx vy)

deflectFromPaddle :: Velocity -> Velocity
deflectFromPaddle v = Coordinates (-Coordinate.x v) (Coordinate.y v)

deflectFromWall :: Velocity -> Velocity
deflectFromWall v = Coordinates (Coordinate.x v) (-Coordinate.y v)
        
-- Test data
start_v2 = Coordinates 1 1
start_p = Coordinates 15.0 15.0
board = Board 123456 (Paddle 19 "foo") (Paddle 14 "bar") (Ball (Coordinates 30 40)) (Conf 640 480  50 10 5 15)
