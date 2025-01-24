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
import Deck(makeRank, defaultDeck, Rank, Suit(Diamonds,Clubs,Hearts,Spades), rankToDefaultChips, Chips)
import Data.Time.Clock
import Data.Fixed (Pico)
import Raylib.Util (WindowResources)
import Raylib.Util.Colors (lightGray, rayWhite)
import Raylib.Types (Texture, Color, KeyboardKey(KeyA))
import RaylibLift
import System
    ( GameInput(..),
      Order(Pos),
      Ginput(GKey, GMouse, GNoInput),
      System',
      initWorld,
      Location(Hand, Deck))
import System.Random
import Data.Ix (Ix(inRange))
import System.Random
import Data.Array.IO
import Control.Monad
import System.Random
import System.Random.Shuffle (shuffle')



-- to be removed later
randomPos :: System' (Int,Int)
randomPos = do
  a <- randNumInRange 0 500
  b <- randNumInRange 0 500
  return (a,b)


shuffleList :: [Entity] -> IO [Entity]
shuffleList l = do
  -- rng <- newStdGen
  -- return $ shuffle' l len rng
  shuffle' l len <$> newStdGen
  where
    len = length l

-- I think that position should be calculated
-- based on window heightXwidth rather than fixed
-- but you could possibly have like an order
createDeck :: [(Rank, Suit)] -> System' ()
createDeck [] = return ()
createDeck ((r,s):remainingDeck) = do
  -- newEntity_ is the same as "_ <- newEntity" 
  p <- randomPos
  _ <- newEntity (s, r, chips, Deck, Pos p)
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

getRandomNrInRange :: Int -> IO Int
getRandomNrInRange i = do
  nr :: Int <- randomIO
  if inRange (0,i) nr
    then return nr
    else getRandomNrInRange i

getRandomElement :: [a] -> IO a
getRandomElement l = do
  nr <- getRandomNrInRange listLength
  return $ l !! nr
  where
    listLength = length l

gameSize :: (Int,Int)
gameSize = (800, 600)

defaultHandSize :: Int
defaultHandSize = 8

numberOfCards :: System' (Int,Int)
numberOfCards = do
  cfold f (0,0)
  where
    f :: (Int,Int) -> Location -> (Int,Int)
    f (n,m) Hand = (n + 1, m)
    f (n,m) Deck = (n,m + 1)

getAllEntitesWithDeck :: System' [Entity]
getAllEntitesWithDeck =
  cfoldM f []
  where
    f :: [Entity] -> (Location, Entity) -> System' [Entity]
    f acc (Deck, ety) = return (ety:acc)
    f acc _ = return acc

takeFirstN ::  Int -> [Entity] -> [Entity]
takeFirstN n = foldl f []
  where
    f acc e =
      if length acc >= n
        then acc
        else e:acc

hasEntity :: [Entity] -> Entity ->  Bool
hasEntity [] _ = False
hasEntity (entity:remainder) e =
  (e == entity) || hasEntity remainder e

moveCards :: Int -> System' ()
moveCards i = do
  ents <- getAllEntitesWithDeck
  l <- liftIO $ shuffleList $ takeFirstN i ents
  cmapM_ $ \(_ :: Location, ety :: Entity) -> do
    when (hasEntity l ety) $ do
      set ety Hand

addCardsToHand :: System'()
addCardsToHand = do
  (nr,_) <- numberOfCards
  if nr >= defaultHandSize
    then return ()
    else moveCards $ defaultHandSize - nr


getDeltaTime :: UTCTime -> UTCTime -> Pico
getDeltaTime a b =
  dt
  where
    p = diffUTCTime a b
    dt = nominalDiffTimeToSeconds p

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
    (Just _) -> drawTxt t (0,0) lightGray

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

randNumInRange :: Int -> Int -> System' Int
randNumInRange m mx = do
  randomRIO (m,mx)

drawCards :: Texture  -> System' ()
drawCards t = do
  cmapM_ f
  where
    orderToTuple (Pos (x,y)) = (x,y)
    f (Hand, ety :: Entity) = do
      r :: Rank <- get ety
      s :: Suit <- get ety
      pos :: Order <- get ety
      drawTxt t (orderToTuple pos) lightGray
      liftIO $ putStrLn $ "Rank Suit" <> show r <> show s
      return ()
    f _ = return ()

drawLoop_ ::  Maybe String -> Bool -> (WindowResources, Texture, Texture) -> System' ()
drawLoop_ st False (w,t,t2) = do
    startDraw
    keyA <- keyD KeyA
    changeInput keyA
    drawT "Wow so cool" 30 40 18 lightGray
    drawMyText t
    clearBckgrnd rayWhite
    drawCards t2
    endDraw
    s <- shouldCloseW
    drawLoop_ st s (w,t,t2)
drawLoop_ _ True (w,_,_) = do
    closeW w
    return ()

initialize :: System' ()
initialize = do
  createDeck defaultDeck
  addInputEntity
  addCardsToHand
  return ()

draw :: System'()
draw = do
  initialize
  st <- inputSystemStr
  w <- createWindow gameSize "my awesome and cool game"
  t <- loadRimage "./sprite.png"
  t2 <- loadRimage "./sprite2.png"
  t3 <- loadRimage "./sprite2.png"
  drawLoop_ st False (w,t,t2)

main :: IO ()
main = do
  putStrLn "hello hello hello"
  w <- initWorld
  runSystem draw w




