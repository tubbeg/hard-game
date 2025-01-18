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
data GameState = MyState -- I can switch this with the ECS

-- this function could update the game loop (ECS)
drawGame :: GameState -> [Widget ()]
drawGame gs =
  [w]
  where
    w :: Widget ()
    w = str "Hello," <=> str "World!" <=> V.Vty.hBorder


eventHandle :: BrickEvent () TickUpdate -> EventM () GameState ()
eventHandle be = do
  return ()


-- attribute maps
-- reminds me a little bit of CSS
dummyMap :: AttrMap
dummyMap = attrMap V.defAttr []

main :: IO ()
main = do
  _ <- defaultMain app initState
  return ()
  where
    initState = MyState
    -- using Unit () as name. Name would have to derive/implement Ord class
    app :: App GameState TickUpdate ()
    app = App {
            appDraw         = drawGame
          , appChooseCursor = neverShowCursor
          , appHandleEvent  = eventHandle
          , appStartEvent   = return () -- do nothing
          , appAttrMap      = const $ dummyMap

          }