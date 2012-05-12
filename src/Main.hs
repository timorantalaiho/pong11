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
  (host:port:name:graphics:_) <- getArgs
  handle <- connectSocket host (read port :: Integer)
  send handle "join" name
  let renderingParams = determineRenderer graphics
  case fst renderingParams of
    True -> do initRenderer
    False -> do return()
  let renderer = snd renderingParams
  handleMessages handle $ renderer

determineRenderer :: String -> (Bool, RendererCommunication)
determineRenderer x
  | x == "true" = (True, startRenderer)
  | otherwise = (False, dummyRenderer)

connectSocket host port = connectTo host (PortNumber $ fromInteger port)

send :: ToJSON a => Handle -> String -> a -> IO ()
send h msgType msgData = do
  let json = encode $ object ["msgType" .= msgType, "data" .= msgData]
  L.hPut h $ json
  hPutChar h '\n'
  hFlush h
  --putStrLn $ ">> " ++ (show json)

handleMessages :: Handle -> RendererCommunication -> IO()
handleMessages h channel = do
  lines <- liftM (L.split '\n') $ L.hGetContents h
  handleLines emptyState h channel lines

handleLines :: State -> Handle -> RendererCommunication -> [L.ByteString] -> IO()
handleLines state h channel [] = do 
  putStrLn "Got empty line from server."
  return ()
handleLines state h channel lines = do
  newstate <- handleLine state h channel $ head lines
  --putStrLn "<< state"
  --putStrLn $ show newstate
  --putStrLn "state >>"
  handleLines newstate h channel $ tail lines

handleLine ::  State -> Handle -> RendererCommunication -> L.ByteString -> IO (State)
handleLine state h channel msg = do
  case decode msg of
    Just json -> do
      writeMsgLogLine (show json)
      let (msgType, msgData) = fromOk $ fromJSON json
      handleMessage state h channel msgType msgData
    Nothing -> do 
      putStrLn $ "Warning, got invalid JSON message. <<" ++ (show msg) ++ ">>"
      return state

sendmessage :: Handle -> CommandHistory -> Int -> Float -> Float -> IO( Maybe(Command))
sendmessage h commandHistory lastMessageTime oldDirection newDirection = do
  let directionChanged = abs(oldDirection - newDirection) > 0.1
      commandsInLastSec = length $ filter (duringLastSec lastMessageTime) commandHistory
      doSend = directionChanged && (commandsInLastSec < 10)
  case doSend of 
    True -> do
      send h "changeDir" newDirection                        
      return (Just(Command lastMessageTime newDirection))                   
    False -> do 
      return (Nothing)
  where 
    duringLastSec :: Int -> Command -> Bool
    duringLastSec newest current = abs(newest - (timestamp current)) < 1100  
    
sendmissile :: Handle -> Board -> Velocity -> Missiles -> IO( Missiles)
sendmissile h board v [] = return ([])
sendmissile h board v (x:xs) = do
  let ly = (leftPaddleMiddleY board)
      ry = (rightPaddleMiddleY board)
      ballGoingAway = Coordinate.x v > 0
      launch = ballGoingAway && abs(ly - ry) < (boardHeight board)
  case launch of 
    True -> do 
      putStrLn $ "LAUCHED MISSILE" ++ (show x)
      send h "launchMissile" x
      return (xs)
    False -> do 
      return (x:xs)

handleMessage :: State -> Handle -> RendererCommunication -> [Char] -> Value -> IO (State)
handleMessage state h channel "gameIsOn" boardJson = do
  let board = fromOk $ GJ.fromJSON boardJson :: Board
      newBoardHistory = board : (boardHistory state)
  logStatistics board
  rendererCommunication <- channel
  putStrLn $ "<< " ++ (show board)
  let oldDirection = (lastDirection $ head $ commandHistory $ state)
      directionResults = calculateDirection newBoardHistory
      newDirection = fst directionResults
      lastMessageTime = (time board)
      oldCommandHistory = (commandHistory state)
      oldMissiles = (missiles state)
      velocity = ballVelocity newBoardHistory
  rendererCommunication (Message (snd directionResults) board)      
  newmissiles <- sendmissile h board velocity oldMissiles
  result <- sendmessage h oldCommandHistory lastMessageTime oldDirection newDirection
  case result of Just(command) -> return $ State (take 5 newBoardHistory) (take 100 $ command : oldCommandHistory) newmissiles
                 Nothing       -> return $ State (take 5 newBoardHistory) (commandHistory state) newmissiles

handleMessage state h channel "missileReady" missilesJson = do
  let missile = fromOk $ GJ.fromJSON missilesJson :: Missile  
  logMissilesReady missile
  return (State (boardHistory state) (commandHistory state) (missile : (missiles state)))

handleMessage state h channel "gameStarted" playersJson = do
  logGameStart playersJson
  return emptyState

handleMessage state h channel "gameIsOver" winnerJson = do
  logGameEnd winnerJson
  return emptyState

handleMessage state h channel anyMessage json = do
  logUnknown anyMessage json
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


