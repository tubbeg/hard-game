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


{-

not exporting the constructor, just the type itself
which means the only way to create a rank is with the
makeRank function. This means that we have guarantees.
I'm really starting to understand why Haskell is awesome

r :: Maybe Rank
r = makeRank 15

r2 :: Rank -- this will fail
r2 = R 100
-}

newtype Order = Pos Int deriving Show
instance Component Order where type Storage Order = Map Order


makeWorld "World" [''Order, ''Rank, ''Suit, ''Chips]

type System' a = System World a


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

initialize :: System' ()
initialize = do
  createDeck defaultDeck
  return ()

getDeltaTime :: UTCTime -> UTCTime -> Pico
getDeltaTime a b =
  dt
  where
    p = diffUTCTime a b
    dt = nominalDiffTimeToSeconds p



ui :: Widget ()
ui = str "Hello, world!"

myApp :: App String Int Int -> String
myApp a = ""

data TickUpdate = Tick
data GameState = DefState | KEYDOWN | MOUSESUPPORT -- I can switch this with the ECS

-- this function could update the game loop (ECS)
drawGame :: GameState -> [Widget ()]
drawGame KEYDOWN =
  [w]
  where
    w :: Widget ()
    w = str "KEY," <=> str "DOWN!" <=> V.Vty.hBorder
drawGame DefState =
  [w]
  where
    w :: Widget ()
    w = str "Hello," <=> str "World!" <=> V.Vty.hBorder
drawGame MOUSESUPPORT =
  [w]
  where
    w :: Widget ()
    w = str "MOUSE," <=> str "OK!" <=> V.Vty.hBorder

updateECS :: GameState -> GameState
updateECS g = g

eventHandle :: BrickEvent () TickUpdate -> EventM () GameState ()
eventHandle (MouseDown n _ _ _) = do
  --it might be easier to use lens here, but for me it's clearer
  --what's happening with MonadState
  s <- MS.get -- get and then update ecs
  MS.put $ updateECS MOUSESUPPORT
  return ()
eventHandle (VtyEvent (V.EvKey _ _)) = do -- this works!
  s <- MS.get
  MS.put $ updateECS KEYDOWN
  return ()
eventHandle (VtyEvent (V.EvMouseDown {})) = do 
  s <- MS.get
  MS.put $ updateECS MOUSESUPPORT
  return ()
eventHandle be = do
  return ()

-- attribute maps
-- reminds me a little bit of CSS
dummyMap :: AttrMap
dummyMap = attrMap V.defAttr []


-- yep, this works really well
setMouseSupport :: EventM () GameState ()
setMouseSupport = do
  vty <- getVtyHandle
  let output = V.outputIface vty
  when (V.supportsMode output V.Mouse) $
      liftIO $ V.setMode output V.Mouse True


main :: IO ()
main = do
  _ <- defaultMain app initState
  return ()
  where
    initState = DefState
    -- using Unit () as name. Name would have to derive/implement Ord class
    app :: App GameState TickUpdate ()
    app = App {
            appDraw         = drawGame
          , appChooseCursor = neverShowCursor
          , appHandleEvent  = eventHandle
          , appStartEvent   = setMouseSupport
          , appAttrMap      = const $ dummyMap
          }