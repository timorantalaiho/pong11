module GameLogic where

import Domain
import Coordinate
import Data.Maybe
import Debug.Trace

calculateDirection :: State -> (Float, [Coordinates])
calculateDirection state = directionToTake
  --Debug.Trace.trace ("DISTANCE: " ++ show ballDistance ++ ", TIME TO HIT:" ++ (show $ timeTakenFor ballDistance v) ++ ", CAN WE MAKES IT?" ++ (show canWeMakeIt)) $  directionToTake
  where history = boardHistory state
        board = head history
        current = leftPaddleMiddleY board
        targetOffset = negate $ ((ballAngle coords) * (paddleH board)) / 2.0
        (uncorrectedTarget, coords) = ballRouteToOurEnd history
        targetToSaveBall = clampTargetPos board $ uncorrectedTarget + targetOffset
        trajectoryFromBall = reverse $ (ballCoordinates board) : reverse coords
        ballDistance = vectorLength trajectoryFromBall
        v = ballVelocity history
        canWeMakeIt = canWeEasilyMakeItToSave board current targetToSaveBall (timeTakenFor ballDistance v)
        directionToSaveBall = chooseDirection current (targetToSaveBall, coords) board
        directionToTake
            | canWeMakeIt = (trollDirection state, snd directionToSaveBall)
            | otherwise = directionToSaveBall

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

vectorLength :: [Coordinates] -> Float
vectorLength (p:[]) = 0.0
vectorLength (p1:p2:ps) = (sqrt (x * x + y * y)) + vectorLength ps
  where x = abs(Coordinate.x p1 - Coordinate.x p2)
        y = abs(Coordinate.y p1 - Coordinate.y p2)
vectorLength _ = 0.0

timeTakenFor :: Float -> Velocity -> Float
timeTakenFor l v = l / (velocityLength)
  where velocityLength = vectorLength [(Coordinates 0 0), v]

dotProduct :: Coordinates -> Coordinates -> Float
dotProduct (Coordinates x1 y1) (Coordinates x2 y2) = x1 * x2 + y1 * y2

chooseDirection :: Float -> (Float, [Coordinates]) -> Board -> (Float, [Coordinates])
chooseDirection currentY (targetY, coords) board
  | difference < (-threshold) = (-1.0, coords)
  | difference >  threshold = ( 1.0, coords)
  | otherwise = (0.0, coords)
  where difference = targetY - currentY
        threshold = (paddleH board) / 8.0

canWeEasilyMakeItToSave :: Board -> Float -> Float -> Float -> Bool
canWeEasilyMakeItToSave board ourPaddleY nextOurHitY timeToImpact = ballIsFarAway && abs(ourPaddleY - nextOurHitY) < (timeToImpact * 0.1)
    where ballIsFarAway = (distanceToUs board > (3 * paddleH board))

trollDirection :: State -> Float
trollDirection state
    | ((length $ missiles state) >= 1) = directionToOtherPaddle
    | otherwise = -1.0 * directionToOtherPaddle
    where board = head $ boardHistory state
          ourPaddleY = leftPaddleMiddleY board
          otherPaddleY = rightPaddleMiddleY board
          directionToOtherPaddle
              | ourPaddleY < otherPaddleY = 1.0
              | ourPaddleY > otherPaddleY = -1.0
              | otherwise = 0.0

ballRouteToOurEnd :: BoardHistory -> (Float, [Coordinates])
ballRouteToOurEnd (b:bs) =
  (Coordinate.y $ head hitPoints, hitPoints)
  where p = ballCoordinates b
        history = (b:bs)
        v = ballVelocity history
        hitPoints = traceBallToOurPaddle p v b []
ballRouteToOurEnd [] = (0.0, [])

traceBallToOurPaddle :: Coordinates -> Velocity -> Board -> [Coordinates] -> [Coordinates]
traceBallToOurPaddle p v board hitPoints
    | ballStopped v = p : hitPoints
    | gameEnded p board = (deflectFromPaddle v) : hitPoints
    | isJust ourPaddleHit = p' : hitPoints
    | otherwise = traceBallToOurPaddle p' v' board (p' : hitPoints)
    where hitTests = [hitsOurPaddle, hitsOpponentPaddle, hitsCeiling, hitsFloor]
          possibleHits = map (\ht -> ht p v board) hitTests
          hits = catMaybes possibleHits
          ourPaddleHit = head possibleHits
          (v',p') = fromMaybe (v,p) $ listToMaybe hits

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
