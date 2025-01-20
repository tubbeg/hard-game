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
import Brick
import qualified Graphics.Vty as V
import qualified Brick.Widgets.Border as V.Vty
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



newtype Order = Pos Int deriving Show
instance Component Order where
  type Storage Order = Map Order



data Ginput =
  GMouse
  | GKey
  | GNoInput
newtype GameInput = Gin Ginput
instance Component GameInput where
  type Storage GameInput = Map GameInput


{-NOTE! DO NOT FORGET ABOUT THIS MACRO
THINGS WILL EXPLODE IF YOU FORGET TO ADD A COMPONENT HERE
-}
makeWorld "World" [''Order, ''Rank, ''Suit, ''Chips, ''GameInput]

type System' a = System World a
type MyECS = System'()

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

-- reads the system and creates graphics based on data
-- ...if that wasn't obvious already
drawGame :: System'() -> [Widget ()]
drawGame system = 
    -- inputSystemStr system
    [w]
    where
      w :: Widget ()
      w = str "KEY," <=> str "DOWN!" <=> V.Vty.hBorder

updateInput :: System'() -> GameInput -> System' ()
updateInput s gi = do
  s -- i think this is equivalent to s >> cmap ... without the do notation
  cmap f
  return ()
  where
    f :: GameInput -> GameInput
    f (Gin GNoInput) = gi
    f ga = ga

eventHandle :: BrickEvent () TickUpdate -> EventM () MyECS ()
eventHandle (MouseDown n _ _ _) = do
  --it might be easier to use lens here, but for me it's clearer
  --what's happening with MonadState
  s <- MS.get -- get and then update ecs
  MS.put $ updateInput s ns
  return ()
  where
    ns = Gin GMouse
eventHandle (VtyEvent (V.EvKey _ _)) = do -- this works!
  s <- MS.get
  MS.put $ updateInput s ns
  return ()
  where
    ns = Gin GKey
eventHandle (VtyEvent (V.EvMouseDown {})) = do
  s <- MS.get
  MS.put $ updateInput s ns
  return ()
  where
    ns =  Gin GMouse
eventHandle _ = do
  return ()

-- attribute maps
-- reminds me a little bit of CSS
dummyMap :: AttrMap
dummyMap = attrMap V.defAttr []



-- yep, this works really well
setMouseSupport :: EventM () MyECS ()
setMouseSupport = do
  s <- MS.get
  vty <- getVtyHandle
  let output = V.outputIface vty
  when (V.supportsMode output V.Mouse) $
      liftIO $ V.setMode output V.Mouse True


{-

I realized (very late) that I actually can't use the Apecs ECS with
Brick because the state can't be monadic. appDraw always
have to return a [Widget ()]. I can't access the state
monad. There's no way to do side effects

I want to make it clear that Brick isn't necessarily bad. But it's very
difficult to work with when your state is monadic.

My guess is that you either have to wrap the entire app inside your monadic
context, or that you use some sort of channel/multithreading. Both options
seems a little bit complicated to me. Maybe it is really simple, but I
haven't found any examples or documentation of the subject. Or maybe
there's a third option that's really easy to implement. But I haven't found
it yet.

h-raylib has a massive advantage in that all operations can be (should be)
performed in the IO monad, which the Apecs System is based on. It's very easy
to do a lift. I started using Brick because I thought that it would be easier
to integrate with the ECS. Also I don't really need a complex UI. But it looks
like h-raylib might be the easier option after all

I could absolutely be wrong here. Maybe there's something obvious I'm missing.

-}

main2 :: IO ()
main2 = do
  _ <- defaultMain app initState
  return ()
  where
    initState = initialize
    -- using Unit () as name. Name would have to derive/implement Ord class
    app :: App MyECS TickUpdate ()
    app = App {
            -- and this is a bit of a problem
            appDraw         = drawGame
          , appChooseCursor = neverShowCursor
          , appHandleEvent  = eventHandle
          , appStartEvent   = setMouseSupport
          , appAttrMap      = const $ dummyMap
          }

loadRimage :: String -> System' Texture
loadRimage s = do
  img <- liftIO $ loadImage s
  liftIO $ loadTextureFromImage img

createWindow :: () -> System' WindowResources
createWindow () = do
  window <- liftIO $ initWindow 500 500 "git gud"
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

drawTxt :: Texture -> Int -> Int -> Color -> System'()
drawTxt t x y c = liftIO $ drawTexture t x y c

clearBckgrnd :: Color -> System'()
clearBckgrnd c = liftIO $ clearBackground c

shouldCloseW :: System' Bool
shouldCloseW = liftIO $ windowShouldClose

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
     
     

