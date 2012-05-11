{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}
import System.Environment(getArgs)

import Network
import Control.Monad
import System.IO(Handle, hFlush, hPutChar)
import Data.Aeson
import qualified Data.Aeson.Generic as GJ
import Data.Maybe
import Data.Data
import Data.Typeable
import qualified Data.ByteString.Lazy.Char8 as L

import Domain
import Coordinate
import GameLogic
import Log
import Render

main = do
  (host:port:name:_) <- getArgs
  handle <- connectSocket host (read port :: Integer)
  send handle "join" name
  initRenderer
  handleMessages handle $ startRenderer

connectSocket host port = connectTo host (PortNumber $ fromInteger port)

send :: ToJSON a => Handle -> String -> a -> IO ()
send h msgType msgData = do
  putStrLn "SENDING MSG"
  let json = encode $ object ["msgType" .= msgType, "data" .= msgData]
  L.hPut h $ json
  hPutChar h '\n'
  hFlush h
  --putStrLn $ ">> " ++ (show json)

handleMessages h channel = do
  lines <- liftM (L.split '\n') $ L.hGetContents h
  handleLines emptyState h channel lines

handleLines state h channel lines = do
  newstate <- handleLine state h channel $ head lines
  putStrLn "<< state"
  putStrLn $ show newstate
  putStrLn "state >>"
  handleLines newstate h channel $ tail lines

handleLine ::  State -> Handle -> RendererCommunication -> L.ByteString -> IO (State)
handleLine state h channel msg = do
  case decode msg of
    Just json -> do
      let (msgType, msgData) = fromOk $ fromJSON json
      handleMessage state h channel msgType msgData
    Nothing -> do 
      putStrLn "Warning, got invalid JSON message."
      return state

sendmessage :: Handle -> Float -> Float -> IO()
sendmessage h oldDirection newDirection 
  | abs(oldDirection - newDirection) > 0.1 = send h "changeDir" newDirection
  | otherwise = putStrLn "nop"

handleMessage :: State -> Handle -> RendererCommunication -> [Char] -> Value -> IO (State)
handleMessage state h channel "gameIsOn" boardJson = do
  let board = fromOk $ GJ.fromJSON boardJson :: Board
      newHistory = board : (boardHistory state)
  logStatistics board
  let directionResults = calculateDirection newHistory
  ch <- channel
  ch (Message (snd directionResults) board)
  let newDirection = fst directionResults
  sendmessage h (lastDirection state) newDirection
  putStrLn $ "<< " ++ (show board)
  putStrLn $ "BALL VELOCITY:" ++ (show $ ballVelocity (boardHistory state))
--  putStrLn $ "CURRENT TRACE:" ++ (show $ traceBallToOurPaddle (extractBallCoordinates board) (ballVelocity state) board)
  return $ State (take 5 newHistory) newDirection

handleMessage state h channel "gameStarted" playersJson = do
  logGameStart playersJson
  return emptyState

handleMessage state h channel "gameIsOver" winnerJson = do
  logGameEnd winnerJson
  return emptyState

handleMessage state h channel anyMessage json = do
  logUnknown json
  return state

instance FromJSON (String, Value) where
  parseJSON (Object v) = do
    msgType <- v .: "msgType"
    msgData <- v .: "data"
    return (msgType, msgData)
  parseJSON x          = fail $ "Not an JSON object: " ++ (show x)

-- JSON helpers --
fromOk (Success x) = x
-- fromOk (Error x) =  x
