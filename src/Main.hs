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

main = do
  (host:port:name:_) <- getArgs
  handle <- connectSocket host (read port :: Integer)
  send handle "join" name
  handleMessages handle

connectSocket host port = connectTo host (PortNumber $ fromInteger port)

send :: ToJSON a => Handle -> String -> a -> IO ()
send h msgType msgData = do
  let json = encode $ object ["msgType" .= msgType, "data" .= msgData]
  L.hPut h $ json
  hPutChar h '\n'
  hFlush h
  --putStrLn $ ">> " ++ (show json)

handleMessages h = do
  lines <- liftM (L.split '\n') $ L.hGetContents h
  handleLines [] h lines

handleLines state h lines = do
  newstate <- handleLine state h $ head lines
  putStrLn "<< state"
  putStrLn $ show newstate
  putStrLn "state >>"
  handleLines newstate h $ tail lines

handleLine ::  State -> Handle -> L.ByteString -> IO (State)
handleLine state h msg = do
  case decode msg of
    Just json -> do
      let (msgType, msgData) = fromOk $ fromJSON json
      handleMessage state h msgType msgData
    Nothing -> do 
      putStrLn "Warning, got invalid JSON message."
      return state

handleMessage :: State -> Handle -> [Char] -> Value -> IO (State)
handleMessage state h "gameIsOn" boardJson = do
  let board = fromOk $ GJ.fromJSON boardJson :: Board
  logStatistics board    
  let direction = calculateDirection board
  send h "changeDir" direction
  putStrLn $ "<< " ++ (show board)
  return $ take 5 $ board : state

handleMessage state h "gameStarted" playersJson = do
  putStrLn $ "<< GAME STARTED WITH: " ++ (show playersJson)
  return state
handleMessage state h "gameEnded" winnerJson = do
  putStrLn $ "<< AND THE WINNER IS: " ++ (show winnerJson)
  return state

handleMessage state h anyMessage json = do
  let direction = 1.0 :: Float
  send h "changeDir" direction
  putStrLn "no-op"
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
