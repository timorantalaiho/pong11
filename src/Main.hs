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
  handleMessages handle $ startRenderer

connectSocket host port = connectTo host (PortNumber $ fromInteger port)

send :: ToJSON a => Handle -> String -> a -> IO ()
send h msgType msgData = do
  let json = encode $ object ["msgType" .= msgType, "data" .= msgData]
  L.hPut h $ json
  hPutChar h '\n'
  hFlush h
  --putStrLn $ ">> " ++ (show json)

handleMessages h channel = do
  lines <- liftM (L.split '\n') $ L.hGetContents h
  handleLines [] h channel lines

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

handleMessage :: State -> Handle -> RendererCommunication -> [Char] -> Value -> IO (State)
handleMessage state h channel "gameIsOn" boardJson = do
  let board = fromOk $ GJ.fromJSON boardJson :: Board
      newState = board : state
  logStatistics board
  ch <- channel
  ch board
  let direction = calculateDirection newState
  send h "changeDir" direction
  putStrLn $ "<< " ++ (show board)
  putStrLn $ "BALL VELOCITY:" ++ (show $ ballVelocity state)
  putStrLn $ "NEXT HIT:" ++ (show $ nextHit state)
  -- putStrLn $ "NEXT HIT:" ++ (show $ nextHit state)
  return $ take 5 $ newState

handleMessage state h channel "gameStarted" playersJson = do
  logGameStart playersJson
  return []

handleMessage state h channel "gameIsOver" winnerJson = do
  logGameEnd winnerJson
  return []

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
