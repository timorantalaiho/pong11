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
import MissileLogic
import Log
import Render
import Message
import Json

main = do
  (host:port:name:graphics:_) <- getArgs
  handle <- connectSocket host (read port :: Integer)
  send handle "join" name
  let renderingParams = determineRenderer graphics
  case fst renderingParams of
    True -> do initRenderer
    False -> do return()
  let renderer = snd renderingParams
  handleConnection handle $ renderer

connectSocket :: String -> Integer -> IO(Handle)
connectSocket host port = connectTo host (PortNumber $ fromInteger port)

determineRenderer :: String -> (Bool, RendererCommunication)
determineRenderer x
  | x == "true" = (True, startRenderer)
  | otherwise = (False, dummyRenderer)

handleConnection :: Handle -> RendererCommunication -> IO()
handleConnection h channel = do
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