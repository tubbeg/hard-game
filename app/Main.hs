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


import Apecs
import Linear
import System.Random
import System.Exit
import Control.Monad
import Data.Monoid
import Data.Semigroup (Semigroup)
import Deck(makeRank, Rank, Suit(Diamonds,Clubs,Hearts,Spades), rankToDefaultChips, Chips)
import Data.Time.Clock
import Data.Fixed (Pico)
import Control.Monad.State.Class as MS
import Graphics.Vty (Input(Input))
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
import RaylibLift
import System

defaultRanks :: [Rank]
defaultRanks =
  foldr f [] rankMaybs
  where
    numbers = [2..14]
    rankMaybs = [makeRank x | x <- numbers] -- list comprehension
    f ::  Maybe Rank -> [Rank] -> [Rank]
    f (Just someRank) l = someRank:l
    f Nothing rankList = rankList

defaultDeck :: [(Rank, Suit)]
defaultDeck =
    spades ++ hearts ++ clubs ++ diamonds
    where
      spades = [(r, Spades) | r <- defaultRanks]
      hearts = [(r, Hearts) | r <- defaultRanks]
      clubs = [(r, Clubs) | r <- defaultRanks]
      diamonds = [(r, Diamonds) | r <- defaultRanks]

createDeck :: [(Rank, Suit)] -> System' ()
createDeck [] = return ()
createDeck ((r,s):remainingDeck) = do
  _ <- newEntity (s, r, chips)
  createDeck remainingDeck
  where
    chips = rankToDefaultChips r

addInputEntity :: System' ()
addInputEntity = do
  _ <- newEntity i
  return ()
  where
    i :: GameInput
    i = Gin GNoInput

initialize :: System' ()
initialize = do
  createDeck defaultDeck
  addInputEntity
  return ()

getDeltaTime :: UTCTime -> UTCTime -> Pico
getDeltaTime a b =
  dt
  where
    p = diffUTCTime a b
    dt = nominalDiffTimeToSeconds p

data TickUpdate = Tick

inputSystemStr :: System' (Maybe String)
inputSystemStr = do
  cfold f Nothing -- standard fold, except with ECS components. I throw away the accumulator
  where
    f :: Maybe String -> GameInput -> Maybe String
    f _ (Gin GKey) = Just "gkey"
    f _ (Gin GMouse) = Just "gmouse"
    f _ _ = Nothing


drawMyText ::Texture -> System'()
drawMyText t = do
  isDoStuff <- inputSystemStr
  case isDoStuff of
    Nothing -> return ()
    (Just _) -> drawTxt t 0 0 lightGray 

changeInput :: Bool -> System'()
changeInput False = do
  cmap f
  return ()
  where
    f :: GameInput -> GameInput
    f _ = Gin GNoInput
changeInput True = do
  cmap f
  return ()
  where
    f :: GameInput -> GameInput
    f (Gin GNoInput) = Gin GKey
    f ga = ga

drawLoop_ ::  Maybe String -> Bool -> (WindowResources, Texture) -> System' ()
drawLoop_ st False (w,t) = do
    startDraw
    keyA <- keyD KeyA
    changeInput keyA
    drawT "Wow so cool" 30 40 18 lightGray
    drawMyText t
    clearBckgrnd rayWhite
    endDraw
    s <- shouldCloseW
    drawLoop_ st s (w,t)
drawLoop_ _ True (w,_) = do  
    closeW w
    return ()

draw :: System'()
draw = do
  initialize
  st <- inputSystemStr 
  w <- createWindow ()
  t <- loadRimage "./sprite.png"
  drawLoop_ st False (w,t)

main :: IO ()
main = do
  putStrLn "hello hello hello"
  w <- initWorld
  runSystem draw w
     
     

