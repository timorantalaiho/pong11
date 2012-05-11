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

import Domain
import Coordinate

type RendererCommunication = IO (Board -> IO ())

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
  gluPerspective 45 (fromIntegral width/fromIntegral height) 0.1 100 
  glMatrixMode gl_MODELVIEW
  glLoadIdentity
  glFlush

drawScene :: IO ()
drawScene = do
  -- clear the screen and the depth buffer
  glClear $ fromIntegral  $  gl_COLOR_BUFFER_BIT
                         .|. gl_DEPTH_BUFFER_BIT
  glLoadIdentity  -- reset view
  glFlush

renderBoard :: Board -> IO ()
renderBoard board = drawScene

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
     GLFW.setWindowTitle "Jeff Molofee's GL Code Tutorial ... NeHe '99"
     -- register the function to do all our OpenGL drawing
     GLFW.setWindowRefreshCallback drawScene
     -- register the funciton called when our window is resized
     GLFW.setWindowSizeCallback resizeScene
     -- register window close handler
     GLFW.setWindowCloseCallback shutdown
     initGL

rendererChannel :: IO (Chan (Board))
rendererChannel = newChan

startRenderer :: RendererCommunication
startRenderer = do
  channel <- rendererChannel
  forkIO $ do
    initRenderer
    forever $ do
      readChan channel >>= print
  return (writeChan channel)


