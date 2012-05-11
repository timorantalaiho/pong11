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

vectorFrom :: Coordinates -> Coordinates -> Coordinates
vectorFrom c1 c2 = Coordinates ((Coordinate.x c1) + (Coordinate.x c2)) ((Coordinate.y c1) + (Coordinate.y c2))

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

nextHit :: State -> Coordinates
nextHit [] = dummyNextHit []
nextHit (b:[]) = dummyNextHit []
nextHit (board:bs) = projectNextHitFrom (board:bs)

projectNextHitFrom :: State -> Coordinates
projectNextHitFrom (b:bs)
  | ballStopped velocity = (Coordinates 0 (Coordinate.y $ extractBallCoordinates b))
  | otherwise = head $ filter (\c -> fst $ (isHit c velocity b)) ballMovements
  where ballMovements = scanl(vectorFrom)(extractBallCoordinates b)(repeat (nextStep (b:bs)))
        velocity = ballVelocity (b:bs)

ballStopped :: Velocity -> Bool
ballStopped v = ((Coordinate.y v) == 0.0) || ((Coordinate.x v) == 0.0)

nextStep :: State -> Velocity
nextStep s = ballVelocity s

isHit :: Coordinates -> Velocity -> Board -> (Bool, Velocity)
isHit c v b = (wouldHit, v)
  where wouldHit = (hitsPaddle c b) || (hitsOtherPaddle c b) || (hitsCeiling c b) || (hitsFloor c b)

hitsPaddle :: Coordinates -> Board -> Bool
hitsPaddle hit board = ((x hit) <= (leftWallX board)) && (insideBoardY hit board)

hitsOtherPaddle :: Coordinates -> Board -> Bool
hitsOtherPaddle hit board = ((x hit) >= (rightWallX board)) && (insideBoardY hit board)

hitsCeiling :: Coordinates -> Board -> Bool
hitsCeiling hit board = (Coordinate.y hit) <= 0.0 && (insideBoardX hit board)

hitsFloor :: Coordinates -> Board -> Bool
hitsFloor hit board = (Coordinate.y hit) >= (boardHeight board) && (insideBoardX hit board)
        
insideBoardX :: Coordinates -> Board -> Bool
insideBoardX hit board = (Coordinate.x hit) >= (leftWallX board) && (Coordinate.x hit) < (rightWallX board)

insideBoardY :: Coordinates -> Board -> Bool        
insideBoardY hit board = (Coordinate.y hit) >= 0.0 && (Coordinate.y hit) < (boardHeight board)
        
nextHits :: State -> [Coordinates]
nextHits state = [dummyNextHit state]

dummyNextHit :: State -> Coordinates
dummyNextHit [] = Coordinates 0 0
dummyNextHit (b:[]) = Coordinates 0 0
dummyNextHit (board:bs) = Coordinates 0 (Coordinate.y $ extractBallCoordinates board)

