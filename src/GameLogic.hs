module GameLogic where

import Domain
import Coordinate
import Data.Maybe
import Debug.Trace
import Vector

type Line = (Coordinates, Coordinates)
data PaddleDefinition = PaddleDefinition { y :: Float, height :: Float, gradient :: Float -> Coordinates  }

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
        ballDistance = trajectoryLength trajectoryFromBall
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

lineLineIntersection :: Line -> Line -> (Float, Float)
lineLineIntersection (base1, direction1) (base2, direction2) =
  (s, t)
  where s = (dotProduct (base2 - base1) perDirection2) / (dotProduct direction1 perDirection2)
        t = (dotProduct (base1 - base2) perDirection1) / (dotProduct direction2 perDirection1)
        perDirection2 = perpendicular direction2
        perDirection1 = perpendicular direction1

trajectoryLength :: [Coordinates] -> Float
trajectoryLength (p:[]) = 0.0
trajectoryLength (p1:p2:ps) = (sqrt (x * x + y * y)) + trajectoryLength ps
  where x = abs(Coordinate.x p1 - Coordinate.x p2)
        y = abs(Coordinate.y p1 - Coordinate.y p2)
trajectoryLength _ = 0.0

timeTakenFor :: Float -> Velocity -> Float
timeTakenFor l v = l / (velocityLength)
  where velocityLength = vectorLength v

chooseDirection :: Float -> (Float, [Coordinates]) -> Board -> (Float, [Coordinates])
chooseDirection currentY (targetY, coords) board
  | difference < (-threshold) = (-1.0, coords)
  | difference >  threshold = ( 1.0, coords)
  | otherwise = (0.0, coords)
  where difference = targetY - currentY
        threshold = (paddleH board) / 8.0

canWeEasilyMakeItToSave :: Board -> Float -> Float -> Float -> Bool
canWeEasilyMakeItToSave board ourPaddleY nextOurHitY timeToImpact
    | ballIsFarAway = abs(ourPaddleY - nextOurHitY) < (timeToImpact * 0.6)
    | otherwise = False
    where ballIsFarAway = (distanceToUs board > (3 * paddleH board))

trollDirection :: State -> Float
trollDirection state
    | ((length $ missiles state) >= 1) = directionToOtherPaddle
    | otherwise = -0.3 * directionToOtherPaddle
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
    | gameEnded p board = (deflectFromEndWall v) : hitPoints
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
hitsOurPaddle p v board =
  hitsVerticalEdge p v board edgeX paddle
  where edgeX = leftWallX board
        paddle = PaddleDefinition (Domain.y $ left board) (paddleH board) (paddleGradient)
        paddleGradient = (\x -> Coordinates (1.0) (x / 200.0))

hitsOpponentPaddle :: Coordinates -> Velocity -> Board -> Maybe (Velocity,Coordinates)
hitsOpponentPaddle p v board =
  hitsVerticalEdge p v board edgeX paddle
  where edgeX = rightWallX board
        paddle = PaddleDefinition (Domain.y $ right board) (paddleH board) (paddleGradient)
        paddleGradient = (\x -> Coordinates (-1.0) (x / 200.0))

hitsCeiling :: Coordinates -> Velocity -> Board -> Maybe (Velocity,Coordinates)
hitsCeiling p v board =
  hitsHorizontalEdge p v board edgeY
  where edgeY = 0.0

hitsFloor :: Coordinates -> Velocity -> Board -> Maybe (Velocity,Coordinates)
hitsFloor p v board =
  hitsHorizontalEdge p v board edgeY
  where edgeY = boardHeight board

hitsVerticalEdge :: Coordinates -> Velocity -> Board -> Float -> PaddleDefinition -> Maybe (Velocity,Coordinates)
hitsVerticalEdge p (Coordinates 0.0 y) board edgeX paddle = Nothing
hitsVerticalEdge p v board edgeX paddle =
  case hit of
    Just p' -> Just (deflectFromVerticalEdge v paddle p', p')
    _ -> Nothing
  where edge = ((Coordinates edgeX 0.0), (Coordinates 0.0 (boardHeight board)))
        hit = hitsEdge p v edge

hitsHorizontalEdge :: Coordinates -> Velocity -> Board -> Float -> Maybe (Velocity,Coordinates)
hitsHorizontalEdge p (Coordinates x 0.0) board edgeY = Nothing
hitsHorizontalEdge p v board edgeY =
  case hit of
    Just p' -> Just(deflectFromWall v, p')
    _ -> Nothing
  where edge = ((Coordinates (leftWallX board) edgeY), (Coordinates edgeLength 0.0))
        edgeLength = (rightWallX board) - (leftWallX board)
        hit = hitsEdge p v edge

hitsEdge :: Coordinates -> Velocity -> Line -> Maybe Coordinates
hitsEdge p v edge
  | timeToImpact > 0.0 && intersection > 0.0 && intersection < 1.0 = Just p'
  | otherwise = Nothing
  where (timeToImpact, intersection) = lineLineIntersection (p, v) edge
        (edgeOrigin, edgeDirection) = edge
        p' = edgeOrigin + (edgeDirection `vscale` intersection)

deflectFromVerticalEdge :: Velocity -> PaddleDefinition -> Coordinates -> Velocity
deflectFromVerticalEdge (Coordinates vx vy) (PaddleDefinition paddleY paddleHeight paddleGradient) (Coordinates px py)
  | isWithinPaddleBounds py = deflectFromPaddle (Coordinates vx vy) (PaddleDefinition paddleY paddleHeight paddleGradient) (Coordinates px py)
  | otherwise = deflectFromEndWall (Coordinates vx vy)
  where isWithinPaddleBounds = (\y -> y >= paddleY && y <= (paddleY + paddleHeight))

deflectFromPaddle (Coordinates vx vy) (PaddleDefinition paddleY paddleHeight paddleGradient) (Coordinates px py) =
  setVectorLength (rotateVector paddleGradientAtImpact angleDiff) (vectorLength (Coordinates vx vy))
  where paddleGradientAtImpact = paddleGradient (py - paddleMiddleY)
        paddleMiddleY = paddleY + (paddleHeight / 2)
        angleDiff = vectorAngle paddleGradientAtImpact - vectorAngle (Coordinates vx vy)

deflectFromEndWall :: Velocity -> Velocity
deflectFromEndWall (Coordinates vx vy) = Coordinates (-vx) vy

deflectFromWall :: Velocity -> Velocity
deflectFromWall (Coordinates vx vy) = Coordinates vx (-vy)

withinBoardHeight :: Float -> Board -> Bool
withinBoardHeight yPos board = yPos >= 0.0 && yPos <= (boardHeight board)

withinGameAreaWidth :: Float -> Board -> Bool
withinGameAreaWidth xPos board = xPos >= (leftWallX board) && xPos <= (rightWallX board)
