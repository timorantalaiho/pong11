module GameLogic where

import Domain
import Coordinate
import Data.Maybe
import Debug.Trace

calculateDirection :: BoardHistory -> (Float, [Coordinates])
calculateDirection history =
  chooseDirection current (target, coords) board
  where board = head history
        current = leftPaddleMiddleY board
        targetOffset = negate $ ((ballAngle coords) * (paddleH board)) / 2.0
        (uncorrectedTarget, coords) = targetY history
        target = clampTargetPos board $ uncorrectedTarget + targetOffset

clampTargetPos :: Board -> Float -> Float
clampTargetPos board yPos
  | yPos < top = top
  | yPos > bottom = bottom
  | otherwise = yPos
    where ph = paddleH board
          half_ph = ph / 2.0
          bh = boardHeight board
          top = half_ph
          bottom = bh - half_ph

ballVelocity :: BoardHistory -> Velocity
ballVelocity (s1:s2:xs) =
  vectorTo newCoordinates oldCoordinates
  where newCoordinates = ballCoordinates s1
        oldCoordinates = ballCoordinates s2
ballVelocity _ = Coordinates 0.0 0.0

ballAngle :: [Coordinates] -> Float
ballAngle (p1:p2:ps) = ballAngleFromVelocity v
  where v = normalizedVector $ vectorTo p1 p2
ballAngle _ = 0.0

ballAngleFromVelocity :: Velocity -> Float
ballAngleFromVelocity v = (asin $ dotProduct nv ourPaddleVector) * 2.0 / pi
  where nv = normalizedVector v
        ourPaddleVector = Coordinates 0.0 1.0

vectorTo :: Coordinates -> Coordinates -> Coordinates
vectorTo (Coordinates x1 y1) (Coordinates x2 y2) =
    Coordinates (x1 - x2) (y1 - y2)

vectorFrom :: Coordinates -> Coordinates -> Coordinates
vectorFrom (Coordinates x1 y1) (Coordinates x2 y2) =
    Coordinates (x1 + x2) (y1 + y2)

normalizedVector :: Coordinates -> Coordinates
normalizedVector (Coordinates x y) = Coordinates (x/len) (y/len)
  where len = sqrt (x*x + y*y)

dotProduct :: Coordinates -> Coordinates -> Float
dotProduct (Coordinates x1 y1) (Coordinates x2 y2) = x1 * x2 + y1 * y2

chooseDirection :: Float -> (Float, [Coordinates]) -> Board -> (Float, [Coordinates])
chooseDirection currentY (targetY, coords) board
  | difference < (-threshold) = (-1.0, coords)
  | difference >  threshold = ( 1.0, coords)
  | otherwise = (0.0, coords)
  where difference = targetY - currentY
        threshold = (paddleH board) / 8.0

targetY :: BoardHistory -> (Float, [Coordinates])
targetY (b:bs) =
  (Coordinate.y $ head hitPoints, hitPoints)
  where p = ballCoordinates b
        history = (b:bs)
        v = ballVelocity history
        hitPoints = traceBallToOurPaddle p v b []
targetY [] = (0.0, [])

traceBallToOurPaddle :: Coordinates -> Velocity -> Board -> [Coordinates] -> [Coordinates]
traceBallToOurPaddle p v board hitPoints
    | ballStopped v = p : hitPoints
    | gameEnded p board = (deflectFromPaddle v) : hitPoints
    | isJust ourPaddleHit = p' : hitPoints
    | otherwise = traceBallToOurPaddle p' v' board (p' : hitPoints)
    where hitTests = [hitsOurPaddle, hitsOpponentPaddle, hitsCeiling, hitsFloor]
          possibleHits = map (\ht -> ht p v board) hitTests
          hit = filter isJust possibleHits
          ourPaddleHit = head possibleHits
          (v',p') = case hit of
              [Just (nv,np)] -> (nv,np)
              _ -> (v,p)

ballStopped :: Velocity -> Bool
ballStopped (Coordinates vx vy) = (vx == 0.0) || (vy == 0.0)

gameEnded :: Coordinates -> Board -> Bool
gameEnded c b = weWon c b || weLost c b
    where weLost (Coordinates x y) board = x < leftWallX board
          weWon (Coordinates x y) board = x > rightWallX board

hitsOurPaddle :: Coordinates -> Velocity -> Board -> Maybe (Velocity,Coordinates)
hitsOurPaddle p (Coordinates 0.0 y) board = Nothing
hitsOurPaddle p v board
    | timeToInpact > 0.0 && withinBoardHeight yPos board = Just (v', p')
    | otherwise = Nothing
    where timeToInpact = (left - (Coordinate.x p)) / (Coordinate.x v)
          left = leftWallX board
          yPos = (Coordinate.y p) + ((Coordinate.y v) * timeToInpact)
          p' = Coordinates left yPos
          v' = deflectFromPaddle v

hitsOpponentPaddle :: Coordinates -> Velocity -> Board -> Maybe (Velocity,Coordinates)
hitsOpponentPaddle p (Coordinates 0.0 y) board = Nothing
hitsOpponentPaddle p v board
    | timeToInpact > 0.0 && withinBoardHeight yPos board = Just (v', p')
    | otherwise = Nothing
    where timeToInpact = (right - (Coordinate.x p)) / (Coordinate.x v)
          right = rightWallX board
          yPos = (Coordinate.y p) + ((Coordinate.y v) * timeToInpact)
          p' = Coordinates right yPos
          v' = deflectFromPaddle v

hitsCeiling :: Coordinates -> Velocity -> Board -> Maybe (Velocity,Coordinates)
hitsCeiling p (Coordinates x 0.0) board = Nothing
hitsCeiling p v board
  | timeToInpact > 0.0 && withinGameAreaWidth xPos board = Just (v', p')
  | otherwise = Nothing
  where timeToInpact = (0.0 - (Coordinate.y p)) / (Coordinate.y v)
        xPos = (Coordinate.x p) + ((Coordinate.x v) * timeToInpact)
        p' = Coordinates xPos 0.0
        v' = deflectFromWall v

hitsFloor :: Coordinates -> Velocity -> Board -> Maybe (Velocity,Coordinates)
hitsFloor p (Coordinates x 0.0) board = Nothing
hitsFloor p v board
  | timeToInpact > 0.0 && withinGameAreaWidth xPos board = Just (v', p')
  | otherwise = Nothing
  where timeToInpact = (height - (Coordinate.y p)) / (Coordinate.y v)
        height = boardHeight board
        xPos = (Coordinate.x p) + ((Coordinate.x v) * timeToInpact)
        p' = Coordinates xPos height
        v' = deflectFromWall v

deflectFromPaddle :: Velocity -> Velocity
deflectFromPaddle (Coordinates vx vy) = Coordinates (-vx) vy

deflectFromWall :: Velocity -> Velocity
deflectFromWall (Coordinates vx vy) = Coordinates vx (-vy)

withinBoardHeight :: Float -> Board -> Bool
withinBoardHeight yPos board = yPos >= 0.0 && yPos <= (boardHeight board)

withinGameAreaWidth :: Float -> Board -> Bool
withinGameAreaWidth xPos board = xPos >= (leftWallX board) && xPos <= (rightWallX board)
        
-- Test data
start_v2 = Coordinates 1 1
start_p = Coordinates 15.0 15.0
board = Board 123456 (Paddle 19 "foo") (Paddle 14 "bar") (Ball (Coordinates 100 100)) (Conf 640 480  50 10 5 15)
board' = Board 123471 (Paddle 19 "foo") (Paddle 14 "bar") (Ball (Coordinates 2 2)) (Conf 640 480  50 10 5 15)
losingHistory = [board',board]

winningBoard = Board 123456 (Paddle 19 "foo") (Paddle 14 "bar") (Ball (Coordinates 500 400)) (Conf 640 480  50 10 5 15)
winningBoard' = Board 123471 (Paddle 19 "foo") (Paddle 14 "bar") (Ball (Coordinates 639 479)) (Conf 640 480  50 10 5 15)
winningHistory = [winningBoard',winningBoard]

