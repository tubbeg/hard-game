module RaylibLift where
import Raylib.Core.Textures
  ( drawTexture,
    drawTexturePro,
    genImagePerlinNoise,
    loadImage,
    loadRenderTexture,
    loadTextureFromImage, loadImageFromTexture,
  )
import Raylib.Core (clearBackground, isKeyDown, beginDrawing, endDrawing, initWindow, setTargetFPS, windowShouldClose, closeWindow)
import Raylib.Core.Text (drawText)
import Raylib.Util (drawing, raylibApplication, WindowResources, managed)
import Raylib.Util.Colors (lightGray, rayWhite)
import Raylib.Types (Texture, Color, KeyboardKey(KeyA))
import Apecs
import System

-- I highly suspect that there is an easier
-- solution than this


loadRimage :: String -> System' Texture
loadRimage s = do
  img <- liftIO $ loadImage s
  liftIO $ loadTextureFromImage img

createWindow :: (Int,Int) -> String -> System' WindowResources
createWindow (x,y) title = do
  window <- liftIO $ initWindow x y title
  liftIO $ setTargetFPS 60
  return window

startDraw :: System'()
startDraw = liftIO beginDrawing

endDraw :: System'()
endDraw = liftIO endDrawing

drawT :: String -> Int -> Int -> Int -> Color -> System'()
drawT s x y sz c = liftIO $ drawText s x y sz c

keyD :: KeyboardKey -> System' Bool
keyD k = liftIO $ isKeyDown k

closeW :: WindowResources -> System'()
closeW w = liftIO $ closeWindow $ Just w

drawTxt :: Texture -> (Int,Int) -> Color -> System'()
drawTxt t (x,y) c = liftIO $ drawTexture t x y c

clearBckgrnd :: Color -> System'()
clearBckgrnd c = liftIO $ clearBackground c

shouldCloseW :: System' Bool
shouldCloseW = liftIO $ windowShouldClose