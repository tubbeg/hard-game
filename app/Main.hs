{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where


import Raylib.Core (clearBackground, beginDrawing, endDrawing, initWindow, setTargetFPS, windowShouldClose, closeWindow)
import Raylib.Core.Text (drawText)
import Raylib.Util (drawing, raylibApplication, WindowResources)
import Raylib.Util.Colors (lightGray, rayWhite)
import Apecs
import Linear
import System.Random
import System.Exit
import Control.Monad
import Data.Monoid
import Data.Semigroup (Semigroup)



newtype Position = Position (V2 Float) deriving Show
instance Component Position where type Storage Position = Map Position
makeWorld "World" [''Position]

type System' a = System World a

initialize :: System' ()
initialize = do
  return ()

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
