{-# LANGUAGE DeriveDataTypeable #-}
module Render where

-- import Graphics.UI.GLUT
import qualified Graphics.UI.GLFW as GLFW
-- everything from here starts with gl or GL
import Graphics.Rendering.OpenGL.Raw
import Graphics.Rendering.GLU.Raw ( gluPerspective )
import Data.Bits ( (.|.) )
import System.Exit ( exitWith, ExitCode(..) )
import Control.Monad ( forever )
import Control.Concurrent
import Control.Concurrent.Chan
import Data.Data
import Debug.Trace

import Domain
import Coordinate
import Missile

-- TODO: Remove Data and Typeable if not necessary
data Message = Message { currentTime :: Int, launched :: [MissileLaunched], hitPoints :: [Coordinates], board :: Board } deriving (Data, Typeable, Show)

type RendererCommunication = IO (Message -> IO ())

initGL :: IO ()
initGL = do
  glShadeModel gl_SMOOTH -- enables smooth color shading
  glClearColor 0 0 0 0 -- Clear the background color to black
  glClearDepth 1 -- enables clearing of the depth buffer
  glEnable gl_DEPTH_TEST
  glDepthFunc gl_LEQUAL -- type of depth test
  glHint gl_PERSPECTIVE_CORRECTION_HINT gl_NICEST

resizeScene :: GLFW.WindowSizeCallback
resizeScene w     0      = resizeScene w 1 -- prevent divide by zero
resizeScene width height = do
  glViewport 0 0 (fromIntegral width) (fromIntegral height)
  glMatrixMode gl_PROJECTION
  glLoadIdentity
  let frustumHeight = (*12) $ tan $ pi / 4
  let frustumWidth = (fromIntegral width/fromIntegral height) * frustumHeight
  glOrtho (negate $ (frustumWidth / 2)) (frustumWidth / 2) (negate $ (frustumHeight / 2)) (frustumHeight / 2) 0.1 100
    -- gluPerspective 45 (fromIntegral width/fromIntegral height) 0.1 100 
  glMatrixMode gl_MODELVIEW
  glLoadIdentity
  glFlush

clearScene :: IO ()
clearScene = do
  -- clear the screen and the depth buffer
  glClear $ fromIntegral  $  gl_COLOR_BUFFER_BIT
                         .|. gl_DEPTH_BUFFER_BIT
  glLoadIdentity  -- reset view
  glFlush

renderBoard :: Message -> IO ()
renderBoard (Message currentTime launched hitPoints board) = do
  -- clear the screen and the depth buffer
  glClear $ fromIntegral  $  gl_COLOR_BUFFER_BIT
                         .|. gl_DEPTH_BUFFER_BIT
  glLoadIdentity  -- reset view
  
  glScalef 0.25 0.25 1.0
  glTranslatef (-20.0) (-10.0) 0.0
  glScalef 0.05 0.05 1

  renderBoardMarkers board
  renderBall board
  renderLeftPaddle board
  renderRightPaddle board
  renderHitPointPairs $ zip hitPoints (tail hitPoints)
  renderMissiles currentTime launched 

  glFlush

renderQuad :: IO ()
renderQuad = do
  glBegin gl_QUADS -- start drawing a polygon (4 sided)
  glVertex3f (-0.5)   0.5  0 -- top left
  glVertex3f   0.5    0.5  0 -- top right
  glVertex3f   0.5  (-0.5) 0 -- bottom right
  glVertex3f (-0.5) (-0.5) 0 -- bottom left
  glEnd

renderBoardMarker :: GLfloat -> GLfloat -> IO()
renderBoardMarker x y = do
  glPushMatrix
  glTranslatef x y (-6.0)
  glScalef 10.0 10.0 1.0
  glColor3f 0.5 0.5 0.5
  renderQuad
  glPopMatrix

renderBoardMarkers :: Board -> IO()
renderBoardMarkers board = do
  renderBoardMarker 0.0 0.0
  renderBoardMarker 640.0 0.0
  renderBoardMarker 640.0 480.0
  renderBoardMarker 0.0 480.0

renderBall :: Board -> IO()
renderBall board = do
  let ballx = realToFrac $ Coordinate.x $ extractBallCoordinates board
      bally = realToFrac $ Coordinate.y $ extractBallCoordinates board
      radius = realToFrac $ ballR board
  glPushMatrix
  glTranslatef ballx bally (-6.0)
  glScalef (radius * 2.5) (radius * 2.5) 1.0
  glColor3f 0.3 0.5 0.9
  renderQuad
  glPopMatrix

renderPaddle :: Board -> Float -> Float -> IO()
renderPaddle board middleX middleY = do
  let width  = paddleW board
      height = paddleH board
  glPushMatrix
  glTranslatef (realToFrac middleX) (realToFrac middleY) (-6.0)
  glScalef (realToFrac width) (realToFrac height) 1.0
  glColor3f 0.2 0.8 0.0
  renderQuad
  glPopMatrix
  
renderRightPaddle :: Board -> IO()
renderRightPaddle board = do
  let width   = paddleW board
      middleY = rightPaddleMiddleY board 
      middleX = (boardWidth board) - (width / 2)
  renderPaddle board middleX middleY

renderLeftPaddle :: Board -> IO()
renderLeftPaddle board = do
  let width   = paddleW board
      middleY = leftPaddleMiddleY board 
      middleX = width / 2
  renderPaddle board middleX middleY

renderHitPointPair :: (Coordinates, Coordinates) -> IO()
renderHitPointPair ((Coordinates xf yf), (Coordinates xt yt)) = do
  glPushMatrix
  glTranslatef (realToFrac xf) (realToFrac yf) (-6.0)
  glScalef 15.0 15.0 1.0
  glColor3f 1.0 0.0 0.0
  renderQuad
  glPopMatrix

  glLineWidth 2.5
  glBegin gl_LINES
  glColor3f 1.0 0.0 0.0
  glVertex3f (realToFrac xf) (realToFrac yf) (-6.0)
  glVertex3f (realToFrac xt) (realToFrac yt) (-6.0)
  glEnd

renderHitPointPairs :: [(Coordinates, Coordinates)] -> IO()
renderHitPointPairs [] = return ()
renderHitPointPairs (x:xs) = do
  renderHitPointPair x
  renderHitPointPairs xs
  
renderMissiles :: Int -> [MissileLaunched] -> IO()
renderMissiles currentTime [] = return ()
renderMissiles currentTime (m:ms) = do
  let startX = missileStartX m
      xCoord = missileCurrentX currentTime m 
      yCoord = Coordinate.y $ Missile.pos $ m
  glColor3f 0.7 0.7 0.2
  glLineWidth 1.5
  glBegin gl_LINES
  glVertex3f (realToFrac startX) (realToFrac yCoord) (-6.0)
  glVertex3f (realToFrac xCoord) (realToFrac yCoord) (-6.0)
  glEnd
  renderMissiles currentTime ms

shutdown :: GLFW.WindowCloseCallback
shutdown = do
  GLFW.closeWindow
  GLFW.terminate
  return True

initRenderer :: IO ()
initRenderer = do
     True <- GLFW.initialize 
     -- get a 640 x 480 window
     let dspOpts = GLFW.defaultDisplayOptions
                     { GLFW.displayOptions_width  = 640
                     , GLFW.displayOptions_height = 480
                     -- Set depth buffering and RGBA colors
                     , GLFW.displayOptions_numRedBits   = 8
                     , GLFW.displayOptions_numGreenBits = 8
                     , GLFW.displayOptions_numBlueBits  = 8
                     , GLFW.displayOptions_numAlphaBits = 8
                     , GLFW.displayOptions_numDepthBits = 1
                     -- , GLFW.displayOptions_displayMode  = GLFW.Fullscreen
                     } 
     -- initialize our window.
     True <- GLFW.openWindow dspOpts
     -- window starts at upper left corner of the screen
     GLFW.setWindowPosition 0 0
     -- open a window
     GLFW.setWindowTitle "pingII++"
     -- register the function to do all our OpenGL drawing
     GLFW.setWindowRefreshCallback clearScene
     -- register the funciton called when our window is resized
     GLFW.setWindowSizeCallback resizeScene
     -- register window close handler
     GLFW.setWindowCloseCallback shutdown
     initGL

rendererChannel :: IO (Chan (Message))
rendererChannel = newChan

startRenderer :: RendererCommunication
startRenderer = do
  channel <- rendererChannel
  forkIO $ do
    forever $ do
      readChan channel >>= renderBoard
      GLFW.swapBuffers
  return (writeChan channel)

dummyRenderer = do
  channel <- rendererChannel
  return (writeChan channel)

