module Log where

import Domain
import Coordinate

logStatistics board = do
  let paddleY = Domain.y $ left $ board
      ballY = Coordinate.y $ pos $ ball $ board
      ballX = Coordinate.x $ pos $ ball $ board
      width = paddleWidth $ conf $ board
      distance = ballX - (fromIntegral width)
  writeLogLine $ show $ (paddleY, ballY, distance)

writeLogLine line = appendFile "/tmp/pong11.log" $ ('\n' : line)
