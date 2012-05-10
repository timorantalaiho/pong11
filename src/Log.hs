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

logGameStart json = do 
  let msg = "<< GAME STARTED WITH: " ++ (show json)
  logLine msg
   
logGameEnd json = do
  let msg = "<< AND THE GAME WINNER IS: " ++ (show json)
  logLine msg

logUnknown json = do
  let msg = "<< UNKNOWN MESSAGE: " ++ (show json)
  logLine msg
  
logLine msg = do
  putStrLn msg
  writeLogLine msg

writeLogLine line = appendFile "/tmp/pong11.log" $ ('\n' : line)
