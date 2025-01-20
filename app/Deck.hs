{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
module Deck (makeRank, Rank, Suit(Clubs, Hearts, Spades, Diamonds), rankToDefaultChips, Chips) where
import Apecs

newtype Rank = R Int
instance Component Rank where
    type Storage Rank = Map Rank

data Suit = Clubs | Diamonds | Hearts | Spades
instance Component Suit where
    type Storage Suit = Map Suit

newtype Chips = C Int
instance Component Chips where
    type Storage Chips = Map Chips
instance Semigroup Chips where
    (<>) (C a) (C b) = C (a + b) --associtivty
    -- the above actually makes sense
    -- we just need to clarify how to values of the type
    -- are to be combined
-- Monoid requries the semigroup class to be instanced
instance Monoid Chips where
    mempty = C 0 --we can use the monoid for the inital value
    -- but this might not be necessary

{-

not exporting the constructor, just the type itself
which means the only way to create a rank is with the
makeRank function. This means that we have guarantees.
I'm really starting to understand why Haskell is awesome
-}
makeRank :: Int -> Maybe Rank
makeRank i
    | i < 15 && i > 1 = Just $ R i
    | otherwise = Nothing


rankToDefaultChips :: Rank -> Chips
rankToDefaultChips (R r)
  | r > 1 && r < 11 = C r
  | r == 15 = C 11
  | otherwise = C 10