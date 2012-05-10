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

main = do
  handle <- connectSocket "localhost" 8080
  let name = "rapala" :: String
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
  handleLines 0 h lines

handleLines state h lines = do
  newstate <- handleLine state h $ head lines
  putStrLn $ show newstate
  handleLines newstate h $ tail lines

handleLine ::  Int -> Handle -> L.ByteString -> IO (Int)
handleLine state h msg = do
  case decode msg of
    Just json -> do
      let (msgType, msgData) = fromOk $ fromJSON json
      handleMessage h msgType msgData
      return (state + 1)
    Nothing -> fail $ "Error parsing JSON: " ++ (show msg)

handleMessage ::Handle -> [Char] -> Value -> IO ()
handleMessage h "chessboard" boardJson = do
  let board = fromOk $ GJ.fromJSON boardJson :: ChessBoard
  putStrLn $ "<< " ++ (show board)

handleMessage h "gameIsOn" boardJson = do
  putStrLn "gameIsOn"
  let board = fromOk $ GJ.fromJSON boardJson :: Board
  putStrLn $ "<< " ++ (show board)

handleMessage h anyMessage json = do
  let direction = 1.0 :: Float
  send h "changeDir" direction
  putStrLn "no-op"

instance FromJSON (String, Value) where
  parseJSON (Object v) = do
    msgType <- v .: "msgType"
    msgData <- v .: "data"
    return (msgType, msgData)
  parseJSON x          = fail $ "Not an JSON object: " ++ (show x)

-- JSON helpers --
fromOk (Success x) = x
-- fromOk (Error x) =  x
