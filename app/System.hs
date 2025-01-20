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
module System (initWorld, System', GameInput(Gin), Ginput(GMouse, GKey, GNoInput)) where
import Deck(makeRank, Rank, Suit(Diamonds,Clubs,Hearts,Spades), rankToDefaultChips, Chips)
import Apecs

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