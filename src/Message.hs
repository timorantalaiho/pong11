{-# LANGUAGE DeriveDataTypeable #-}
module Message where

import Data.Data
import Data.Typeable

import Network
import Data.Aeson
import System.IO(Handle, hFlush, hPutChar)
import qualified Data.Aeson.Generic as GJ

import Domain 
import Render
import Log
import Missile
import MissileLogic
import GameLogic
import Json

handleMessage :: State -> Handle -> RendererCommunication -> [Char] -> Value -> IO (State)
handleMessage state h channel "gameIsOn" boardJson = do
  let board = fromOk $ GJ.fromJSON boardJson :: Board
      newBoardHistory = take 5 $ board : (boardHistory state)
      oldDirection = lastDirection $ head $ commandHistory $ state
      calculationResults = calculateDirection (State newBoardHistory (commandHistory state) oldMissiles launched)
      newDirection = direction calculationResults
      lastMessageTime = time board
      oldCommandHistory = commandHistory state
      oldMissiles = missiles state
      velocity = ballVelocity newBoardHistory
      wayPointsWithBall = reverse $ (ballCoordinates board : (reverse $ wayPoints calculationResults))
      launched = removeMissedMissiles lastMessageTime (boardWidth board) (launchedMissiles state)
  logStatistics board
  rendererCommunication <- channel
  rendererCommunication (Message lastMessageTime launched wayPointsWithBall board)
  newmissiles <- sendmissile h board velocity wayPointsWithBall oldMissiles
  result <- sendmessage h oldCommandHistory lastMessageTime oldDirection newDirection
  case result of Just(command) -> return $ State newBoardHistory (take 100 $ command : oldCommandHistory) newmissiles launched
                 Nothing       -> return $ State newBoardHistory (commandHistory state) newmissiles launched

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

fromOk (Success x) = x

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
