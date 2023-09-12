{- |
Module      : Cards
Description : A model of playing cards
Copyright   : (c) TDA555/DIT441, Introduction to Functional Programming
License     : BSD
Maintainer  : alexg@chalmers.se
Stability   : experimental

Authors     : <list your names here>
Lab group   : <group number>
-}

module Blackjack where

import Cards
import RunGame
import Data.List
import System.Random
import Test.QuickCheck hiding (shuffle)

hand :: Hand
hand = [
  Card (Numeric 2) Spades,
  Card Ace Clubs,
  Card King  Hearts,
  Card Ace Diamonds,
  Card Queen Hearts ]

hand2 :: Hand
hand2 = [
  Card (Numeric 2) Spades,
  Card Jack Spades ] 

sizeSteps :: [Int]
sizeSteps = [ 
  size hand2,
  size (Card (Numeric 2) Hearts : (Card Jack Spades : [])),
  1 + (size [(Card Jack Spades)]),
  1 + 1 + size [],
  1 + 1 + 0,
  2 ]

-- A2
getRank :: Card -> String 
getRank (Card (Numeric n) _) = show n 
getRank (Card r _) = show r

getSuit :: Card -> String
getSuit (Card _ s) = show s

displayCard :: Card -> String
displayCard card = (getRank card) ++ " of " ++ (getSuit card) 

display :: Hand -> String
display = intercalate ", " . map displayCard 

-- A3
valueCard :: Card -> Int
valueCard (Card Ace _) = 11
valueCard (Card (Numeric n) _) = n 
valueCard (Card _ _) = 10

-- Auto-soft all aces
numberOfAces :: Hand -> Int
numberOfAces = length . filter (==11) . map valueCard

value :: Hand -> Int
value [] = 0
value hand  
  | (rawCount > 21) = (rawCount - (numberOfAces hand) * 10)
  | otherwise = rawCount
  where rawCount = sum $ map valueCard hand

-- Auto-soft aces until under 21
aceSearch :: [Int] -> Int -> [Int]
aceSearch [] _ = []
aceSearch (x:xs) count = 
  if (x == 11 && count > 21) then
    1 : (aceSearch xs (count - 10))
  else
    x : (aceSearch xs count)

value2 :: Hand -> Int
value2 hand = sum $ aceSearch cardValues (sum cardValues)
  where cardValues = map valueCard hand

-- A4
gameOver :: Hand -> Bool
gameOver hand = (value hand) > 21

-- B1
allRanks :: [Rank]
allRanks = [Numeric n | n <- [2..10]] ++ [Jack, Queen, King, Ace]

allSuits :: [Suit]
allSuits = [Spades, Hearts, Diamonds, Clubs]

fullDeck :: Deck
fullDeck = [(Card r s) | r <- allRanks, s <- allSuits]

prop_size_fullDeck :: Bool
prop_size_fullDeck = size fullDeck == 52

-- B2
draw :: Deck -> Hand -> (Deck, Hand)
draw [] _ = error "draw: The deck is empty."
draw (x:xs) h = (xs, (x : h))

-- B3
playBank :: Deck -> Hand
playBank deck = fst $ playBank' deck []

playBank' :: Deck -> Hand -> (Deck, Hand)
playBank' deck bankHand
  | (value bankHand') >= 16 = (deck', bankHand')
  | otherwise = playBank' deck' bankHand'
  where (deck', bankHand') = draw deck bankHand

-- B4
shuffle :: [Double] -> Deck -> Deck
shuffle rl d = shuffle' rl d [] 

shuffle' :: [Double] -> Deck -> Deck -> Deck
shuffle' (x:xs) d1 d2
  | (d1 == []) = d2
  | otherwise = shuffle' xs (fst splitDeck ++ tail(snd splitDeck)) ((d1 !! i) : d2)
  where i = floor $ x * (fromIntegral $ (length d1) - 1)
        splitDeck = splitAt i d1

-- B4 (solution 2)
exchange :: Int -> Int -> Deck -> Deck
exchange i j a = map (\x ->
  if (x == i) then (a !! j)
  else if (x == j) then (a !! i)
  else (a !! x)
  ) [0..(length a - 1)]


