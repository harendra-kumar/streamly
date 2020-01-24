-- Adapted from the Yampa package.
-- Displays a square moving in a circle. To move the position drag it with the
-- mouse.
--
-- Requires the SDL package, assuming streamly has already been built, you can
-- compile it like this:
-- stack ghc --package SDL CirclingSquare.hs

import Data.IORef
import Graphics.UI.SDL as SDL
import Streamly
import Streamly.Prelude as S

------------------------------------------------------------------------------
-- SDL Graphics Init
------------------------------------------------------------------------------

sdlInit :: IO ()
sdlInit = do
  SDL.init [InitVideo]

  let width  = 640
      height = 480
  _ <- SDL.setVideoMode width height 16 [SWSurface]
  SDL.setCaption "Test" ""

------------------------------------------------------------------------------
-- Display a box at a given coordinates
------------------------------------------------------------------------------

display :: (Double, Double) -> IO ()
display (playerX, playerY) = do
  screen <- getVideoSurface

  -- Paint screen green
  let format = surfaceGetPixelFormat screen
  bgColor <- mapRGB format 55 60 64
  _ <- fillRect screen Nothing bgColor

  -- Paint small red square, at an angle 'angle' with respect to the center
  foreC <- mapRGB format 212 108 73
  let side = 20
      x = round playerX
      y = round playerY
  _ <- fillRect screen (Just (Rect x y side side)) foreC

  -- Double buffering
  SDL.flip screen

------------------------------------------------------------------------------
-- Wait and update Controller Position if it changes
------------------------------------------------------------------------------

updateController :: IORef (Double, Double) -> IO ()
updateController ref = do
    e <- pollEvent
    case e of
        MouseMotion x y _ _ -> writeIORef ref (fromIntegral x, fromIntegral y)
        _ -> return ()

------------------------------------------------------------------------------
-- Periodically refresh the output display
------------------------------------------------------------------------------

updateDisplay :: IORef (Double, Double) -> IO ()
updateDisplay cref = do
    time <- SDL.getTicks
    (x, y) <- readIORef cref
    let t = fromIntegral time * speed / 1000
     in display (x + cos t * radius, y + sin t * radius)

    where

    speed  = 6
    radius = 60

main :: IO ()
main = do
    sdlInit
    cref <- newIORef (0,0)
    S.drain $ asyncly $ constRate 40
        $ S.repeatM (updateController cref)
              `parallel` S.repeatM (updateDisplay cref)
