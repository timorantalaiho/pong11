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
--targetY state = Coordinate.y $ Domain.pos $ Domain.ball board
--  where board = head state  
targetY state =
    Coordinate.y $ traceBallToOurPaddle p v b
  where b = head state
        p = extractBallCoordinates b
        v = ballVelocity state

traceBallToOurPaddle :: Coordinates -> Velocity -> Board -> Coordinates
traceBallToOurPaddle p v board
    | ballStopped v = p
    | fst myPaddleHit = p'
    | otherwise = traceBallToOurPaddle p' v' board
    where p' = advanceBall p v
          possibleHits = [myPaddleHit, otherPaddleHit, ceilingHit, floorHit]
          wouldHit = foldl (||) False (map fst possibleHits)
          myPaddleHit = hitsPaddle p' v board
          otherPaddleHit = hitsOtherPaddle p' v board
          ceilingHit = hitsCeiling p' v board
          floorHit = hitsFloor p' v board
          v'
           | wouldHit = snd $ head $ filter fst possibleHits
           | otherwise = v

advanceBall :: Coordinates -> Velocity -> Coordinates
advanceBall p v =
    vectorFrom p v

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
isHit c v b = (wouldHit, v')
  where possibleHits = [myPaddleHit, otherPaddleHit, ceilingHit, floorHit]
        wouldHit = foldl (||) False (map fst possibleHits)
        myPaddleHit = hitsPaddle c v b
        otherPaddleHit = hitsOtherPaddle c v b
        ceilingHit = hitsCeiling c v b
        floorHit = hitsFloor c v b
        v'
          | wouldHit = snd $ head $ filter fst possibleHits
          | otherwise = v

hitsPaddle :: Coordinates -> Velocity -> Board -> (Bool, Velocity)
hitsPaddle hit v board = (isHit, v')
    where isHit = ((x hit) <= (leftWallX board)) && (insideBoardY hit board)
          v'
            | isHit = (Coordinates (-Coordinate.x v) (Coordinate.y v))
            | otherwise = v

hitsOtherPaddle :: Coordinates -> Velocity -> Board -> (Bool, Velocity)
hitsOtherPaddle hit v board = (isHit, v')
    where isHit = ((x hit) >= (rightWallX board)) && (insideBoardY hit board)
          v'
            | isHit = (Coordinates (-Coordinate.x v) (Coordinate.y v))
            | otherwise = v

hitsCeiling :: Coordinates -> Velocity -> Board -> (Bool, Velocity)
hitsCeiling hit v board = (isHit, v')
    where isHit = (Coordinate.y hit) <= 0.0 && (insideBoardX hit board)
          v'
            | isHit = (Coordinates (Coordinate.x v) (-Coordinate.y v))
            | otherwise = v

hitsFloor :: Coordinates -> Velocity -> Board -> (Bool, Velocity)
hitsFloor hit v board = (isHit, v')
    where isHit = (Coordinate.y hit) >= (boardHeight board) && (insideBoardX hit board)
          v'
            | isHit = (Coordinates (Coordinate.x v) (-Coordinate.y v))
            | otherwise = v
        
insideBoardX :: Coordinates -> Board -> Bool
insideBoardX hit board = True --(Coordinate.x hit) >= (leftWallX board) && (Coordinate.x hit) < (rightWallX board)

insideBoardY :: Coordinates -> Board -> Bool        
insideBoardY hit board = True --(Coordinate.y hit) >= 0.0 && (Coordinate.y hit) < (boardHeight board) 
        
nextHits :: State -> [Coordinates]
nextHits state = [dummyNextHit state]

dummyNextHit :: State -> Coordinates
dummyNextHit [] = Coordinates 0 0
dummyNextHit (b:[]) = Coordinates 0 0
dummyNextHit (board:bs) = Coordinates 0 (Coordinate.y $ extractBallCoordinates board)

-- Test data
start_v2 = Coordinates 1 1
start_p = Coordinates 15.0 15.0
board = Board 123456 (Paddle 19 "foo") (Paddle 14 "bar") (Ball (Coordinates 30 40)) (Conf 640 480  50 10 5 15)
