module GameLogic where

import Domain
import Coordinate
import Data.Maybe
import Debug.Trace

type Line = (Coordinates, Coordinates)
data PaddleDefinition = PaddleDefinition { y :: Float, height :: Float, gradient :: Float -> Coordinates  }

instance Eq Coordinates where
  (==) (Coordinates x1 y1) (Coordinates x2 y2) = (x1 == x2) && (y1 == y2)

instance Num Coordinates where
  (+) (Coordinates x1 y1) (Coordinates x2 y2) = Coordinates (x1 + x2) (y1 + y2)
  (-) (Coordinates x1 y1) (Coordinates x2 y2) = Coordinates (x1 - x2) (y1 - y2)  
  (*) (Coordinates x1 y1) (Coordinates x2 y2) = error "Multiplication between two coordinates undefined"
  abs (Coordinates x y) = (Coordinates (abs x) (abs y))
  signum (Coordinates x1 y1) = error "signum of coordinates undefined"
  fromInteger i = (Coordinates (fromInteger i) (fromInteger i))

vscale :: Coordinates -> Float -> Coordinates
vscale (Coordinates vx vy) s = Coordinates (vx * s) (vy * s)

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

perpendicular :: Coordinates -> Coordinates
perpendicular (Coordinates vx vy) = Coordinates (negate vy) vx

lineLineIntersection :: Line -> Line -> (Float, Float)
lineLineIntersection (base1, direction1) (base2, direction2) =
  (s, t)
  where s = (dotProduct (base2 - base1) perDirection2) / (dotProduct direction1 perDirection2)
        t = (dotProduct (base1 - base2) perDirection1) / (dotProduct direction2 perDirection1)
        perDirection2 = perpendicular direction2
        perDirection1 = perpendicular direction1

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
    Just p' -> Just (deflectFromPaddle v paddle p', p')
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

deflectFromPaddle :: Velocity -> PaddleDefinition -> Coordinates -> Velocity
deflectFromPaddle (Coordinates vx vy) paddle position = Coordinates (-vx) vy

deflectFromEndWall :: Velocity -> Velocity
deflectFromEndWall (Coordinates vx vy) = Coordinates (-vx) vy

deflectFromWall :: Velocity -> Velocity
deflectFromWall (Coordinates vx vy) = Coordinates vx (-vy)

withinBoardHeight :: Float -> Board -> Bool
withinBoardHeight yPos board = yPos >= 0.0 && yPos <= (boardHeight board)

withinGameAreaWidth :: Float -> Board -> Bool
withinGameAreaWidth xPos board = xPos >= (leftWallX board) && xPos <= (rightWallX board)
