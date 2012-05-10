module GameLogic where

import Domain
import Coordinate

calculateDirection :: State -> Float
calculateDirection state =
  chooseDirection (paddleMiddleY board) targetY
  where board = head state
        targetY = Coordinate.y $ Domain.pos $ Domain.ball board

paddleMiddleY :: Board -> Float
paddleMiddleY board =
  paddleY + ((fromIntegral $ Domain.paddleHeight $ Domain.conf board) / 2.0)
  where paddleY = Domain.y $ Domain.left board

chooseDirection :: Float -> Float -> Float
chooseDirection currentY targetY
  | difference < 0.0 = -1.0
  | difference > 0.0 = 1.0
  | otherwise = 0.0
  where difference = targetY - currentY
