module Log where

import Domain
import Coordinate

logStatistics board = do
  let paddleY = Domain.y $ left $ board
      ballY = Coordinate.y $ pos $ ball $ board
  writeLogLine $ show $ [paddleY, ballY]

writeLogLine line = appendFile "/tmp/pong11.log" $ ('\n' : line)
