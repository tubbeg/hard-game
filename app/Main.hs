module Main where


import Raylib.Core (clearBackground, beginDrawing, endDrawing, initWindow, setTargetFPS, windowShouldClose, closeWindow)
import Raylib.Core.Text (drawText)
import Raylib.Util (drawing, raylibApplication, WindowResources)
import Raylib.Util.Colors (lightGray, rayWhite)


createWindow :: () -> IO WindowResources
createWindow () = do
  window <- initWindow 600 450 "git gud"
  setTargetFPS 60
  return window

drawLoop :: Bool -> WindowResources -> IO ()
drawLoop False w = do
    beginDrawing
    drawText "Wow so cool" 30 40 18 lightGray
    clearBackground rayWhite
    endDrawing
    s <- windowShouldClose
    drawLoop s w
drawLoop True w = do  
    closeWindow $ Just w
    return ()

main :: IO ()
main = do
  putStrLn "hello hello hello"
  w <- createWindow ()
  drawLoop False w
