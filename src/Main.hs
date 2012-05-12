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
import Missile
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
  writeStateLogLine newstate
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
    
sendmissile :: Handle -> Board -> Velocity -> [Coordinates] -> Missiles -> IO( Missiles)
sendmissile h board v [] [] = return ([])
sendmissile h board v  _ [] = return ([])
sendmissile h board v []  _ = return ([])
sendmissile h board v wayPoints (x:xs) = do
  let ly = (leftPaddleMiddleY board)
      ry = (rightPaddleMiddleY board)
      --ballGoingAway = Coordinate.x v > 0
      rightHit = filter (\w -> abs(ly - (Coordinate.x w)) < 10) wayPoints
      enemyDir = enemyDirection ry rightHit
      --hasRightHit = length rightHit > 0
      --launch = ballGoingAway && hasRightHit && abs(ly - (Coordinate.y $ head rightHit)) < 50
      --launch = ballGoingAway && abs(ly -ry) < 50
      --launch = abs(ly - ry) < 10
      launch = isCloseEnough ly ry enemyDir
  case launch of 
    True -> do 
      send h "launchMissile" x
      return (xs)
    False -> do 
      return (x:xs)

enemyDirection :: Float -> [Coordinates] -> Float
enemyDirection curr [] = 0.0
enemyDirection curr (next:xs)
  | curr - Coordinate.y next > 0 = -(1.0)
  | otherwise = 1.0

isCloseEnough :: Float -> Float -> Float -> Bool
isCloseEnough ly ry enemyDirection
  | enemyDirection == 0.0 = abs(ly - ry) < 10
  | enemyDirection > 0.0 = (ly - ry > 10) && (ly - ry < 50)
  | enemyDirection < 0.0 = (ly - ry < 10) && (ly - ry > -50)

handleMessage :: State -> Handle -> RendererCommunication -> [Char] -> Value -> IO (State)
handleMessage state h channel "gameIsOn" boardJson = do
  let board = fromOk $ GJ.fromJSON boardJson :: Board
      newBoardHistory = board : (boardHistory state)
  logStatistics board
  rendererCommunication <- channel
  let oldDirection = (lastDirection $ head $ commandHistory $ state)
      directionResults = calculateDirection newBoardHistory
      newDirection = fst directionResults
      lastMessageTime = (time board)
      oldCommandHistory = (commandHistory state)
      oldMissiles = (missiles state)
      velocity = ballVelocity newBoardHistory
      wayPoints = reverse $ (ballCoordinates board : (reverse $ snd directionResults))
      launched = removeMissedMissiles lastMessageTime (boardWidth board) (launchedMissiles state)
  rendererCommunication (Message lastMessageTime launched wayPoints board)
  newmissiles <- sendmissile h board velocity wayPoints oldMissiles
  result <- sendmessage h oldCommandHistory lastMessageTime oldDirection newDirection
  case result of Just(command) -> return $ State (take 5 newBoardHistory) (take 100 $ command : oldCommandHistory) newmissiles launched
                 Nothing       -> return $ State (take 5 newBoardHistory) (commandHistory state) newmissiles launched

handleMessage state h channel "missileReady" missilesJson = do
  let missile = fromOk $ GJ.fromJSON missilesJson :: Missile  
  logMissilesReady missile
  return (State (boardHistory state) (commandHistory state) (missile : (missiles state)) (launchedMissiles state))

handleMessage state h channel "missileLaunched" launchedJson = do
  let newLaunch = fromOk $ GJ.fromJSON launchedJson :: MissileLaunched
  logMissilesLaunched newLaunch
  return (State (boardHistory state) (commandHistory state) (missiles state) (newLaunch : (launchedMissiles state)))

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

