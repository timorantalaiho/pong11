module GameLogic where

import Domain
import Coordinate

calculateDirection :: Board -> Float
calculateDirection board =
  chooseDirection $ (ballY - (paddleMiddleY $ board))
  where ballY = Coordinate.y $ Domain.pos $ Domain.ball board

paddleMiddleY :: Board -> Float
paddleMiddleY board =
  paddleY + ((fromIntegral $ Domain.paddleHeight $ Domain.conf board) / 2.0)
  where paddleY = Domain.y $ Domain.left board

chooseDirection :: Float -> Float
chooseDirection difference
  | difference < 0.0 = -1.0
  | difference > 0.0 = 1.0
  | otherwise = 0.0
