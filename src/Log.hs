module Log where

import Domain
import Coordinate

logStatistics board = do
  writeLogLine $ show board

logMissilesReady json = do 
  let msg = "<< MISSILES READY: " ++ (show json)
  logLine msg

logMissilesLaunched json = do 
  let msg = "<< MISSILES LAUNCHED: " ++ (show json)
  logLine msg

logGameStart json = do 
  let msg = "<< GAME STARTED WITH: " ++ (show json)
  logLine msg
   
logGameEnd json = do
  let msg = "<< AND THE GAME WINNER IS: " ++ (show json)
  logLine msg

logUnknown mType json = do
  let msg = "<< UNKNOWN MESSAGE: msgType '" ++ (show mType) ++ "' : " ++ (show json)
  logLine msg
  
logLine msg = do
  putStrLn msg
  writeLogLine msg

writeLogLine line = appendFile "/tmp/pong11.log" $ ('\n' : line)

writeMsgLogLine line = appendFile "/tmp/pong11-messages.log" $ ('\n' : line)

writeStateLogLine state = appendFile "/tmp/pong11-state.log" $ ('\n' : (show state))
