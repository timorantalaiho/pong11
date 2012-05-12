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
        -- Compute target without offset
        -- (target, coords) = targetY history
        -- Compute target WITH offset
        (uncorrectedTarget, coords) = targetY history
        target = clampTargetPos board $ uncorrectedTarget + targetOffset

clampTargetPos :: Board -> Float -> Float
clampTargetPos board yPos
  | yPos < top = top
  | yPos > bottom = bottom
  | otherwise = yPos
    where ph = paddleH board
          bh = boardHeight board
          top = ph / 2.0
          bottom = bh - (ph / 2.0)

ballVelocity :: BoardHistory -> Velocity
ballVelocity [] = Coordinates 0.0 0.0
ballVelocity [x] = Coordinates 0.0 0.0
ballVelocity (s1:s2:xs) =
  vectorTo newCoordinates oldCoordinates
  where newCoordinates = ballCoordinates s1
        oldCoordinates = ballCoordinates s2

ballAngle :: [Coordinates] -> Float
ballAngle (p1:p2:ps) = ballAngleFromVelocity v
  where v = normalizedVector $ vectorTo p1 p2
ballAngle _ = trace "OOO> Less than two coords!" 0.0

ballAngleFromVelocity :: Velocity -> Float
ballAngleFromVelocity v = (asin $ dotProduct nv ourPaddleVector) * 2.0 / pi
  where nv = normalizedVector v

vectorTo :: Coordinates -> Coordinates -> Coordinates
vectorTo c1 c2 = Coordinates ((Coordinate.x c1) - (Coordinate.x c2)) ((Coordinate.y c1) - (Coordinate.y c2))

vectorFrom :: Coordinates -> Coordinates -> Coordinates
vectorFrom c1 c2 = Coordinates ((Coordinate.x c1) + (Coordinate.x c2)) ((Coordinate.y c1) + (Coordinate.y c2))

normalizedVector :: Coordinates -> Coordinates
normalizedVector (Coordinates x y) = Coordinates (x/len) (y/len)
  where len = sqrt (x*x + y*y)

ourPaddleVector = Coordinates 0.0 1.0

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
targetY (x:xs) =
  ((Coordinate.y $ head hitPoints), hitPoints)
  where b = x
        p = ballCoordinates b
        v = ballVelocity (x:xs)
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
ballStopped v = ((Coordinate.y v) == 0.0) || ((Coordinate.x v) == 0.0)

weLost :: Coordinates -> Board -> Bool
weLost ballCoordinates board = Coordinate.x ballCoordinates < leftWallX board

weWon :: Coordinates -> Board -> Bool
weWon ballCoordinates board = Coordinate.x ballCoordinates > rightWallX board

gameEnded :: Coordinates -> Board -> Bool
gameEnded c b = weWon c b || weLost c b

hitsOurPaddle :: Coordinates -> Velocity -> Board -> Maybe (Velocity,Coordinates)
hitsOurPaddle p (Coordinates 0.0 y) board = Nothing
hitsOurPaddle p v board
    | timeToInpact > 0.0 && yPos >= 0.0 && yPos <= (boardHeight board) = Just (v', p')
    | otherwise = Nothing
    where timeToInpact = (left - (Coordinate.x p)) / (Coordinate.x v)
          left = leftWallX board
          yPos = (Coordinate.y p) + ((Coordinate.y v) * timeToInpact)
          p' = Coordinates left yPos
          v' = deflectFromPaddle v

hitsOpponentPaddle :: Coordinates -> Velocity -> Board -> Maybe (Velocity,Coordinates)
hitsOpponentPaddle p (Coordinates 0.0 y) board = Nothing
hitsOpponentPaddle p v board
    | timeToInpact > 0.0 && yPos >= 0.0 && yPos <= (boardHeight board) = Just (v', p')
    | otherwise = Nothing
    where timeToInpact = (right - (Coordinate.x p)) / (Coordinate.x v)
          right = rightWallX board
          yPos = (Coordinate.y p) + ((Coordinate.y v) * timeToInpact)
          p' = Coordinates right yPos
          v' = deflectFromPaddle v

hitsCeiling :: Coordinates -> Velocity -> Board -> Maybe (Velocity,Coordinates)
hitsCeiling p (Coordinates x 0.0) board = Nothing
hitsCeiling p v board
  | timeToInpact > 0.0 && xPos >= (leftWallX board) && xPos <= (rightWallX board) = Just (v', p')
  | otherwise = Nothing
  where timeToInpact = (0.0 - (Coordinate.y p)) / (Coordinate.y v)
        xPos = (Coordinate.x p) + ((Coordinate.x v) * timeToInpact)
        p' = Coordinates xPos 0.0
        v' = deflectFromWall v

hitsFloor :: Coordinates -> Velocity -> Board -> Maybe (Velocity,Coordinates)
hitsFloor p (Coordinates x 0.0) board = Nothing
hitsFloor p v board
  | timeToInpact > 0.0 && xPos >= (leftWallX board) && xPos <= (rightWallX board) = Just (v', p')
  | otherwise = Nothing
  where timeToInpact = (height - (Coordinate.y p)) / (Coordinate.y v)
        height = boardHeight board
        xPos = (Coordinate.x p) + ((Coordinate.x v) * timeToInpact)
        p' = Coordinates xPos height
        v' = deflectFromWall v

deflectFromPaddle :: Velocity -> Velocity
deflectFromPaddle v = Coordinates (-Coordinate.x v) (Coordinate.y v)

deflectFromWall :: Velocity -> Velocity
deflectFromWall v = Coordinates (Coordinate.x v) (-Coordinate.y v)
        
-- Test data
start_v2 = Coordinates 1 1
start_p = Coordinates 15.0 15.0
board = Board 123456 (Paddle 19 "foo") (Paddle 14 "bar") (Ball (Coordinates 100 100)) (Conf 640 480  50 10 5 15)
board' = Board 123471 (Paddle 19 "foo") (Paddle 14 "bar") (Ball (Coordinates 2 2)) (Conf 640 480  50 10 5 15)
losingHistory = [board',board]

winningBoard = Board 123456 (Paddle 19 "foo") (Paddle 14 "bar") (Ball (Coordinates 500 400)) (Conf 640 480  50 10 5 15)
winningBoard' = Board 123471 (Paddle 19 "foo") (Paddle 14 "bar") (Ball (Coordinates 639 479)) (Conf 640 480  50 10 5 15)
winningHistory = [winningBoard',winningBoard]

