{- |
Module      : RunGame
Description : Some function to run a user defined card game
Copyright   : (c) TDA555/DIT441, Introduction to Functional Programming
License     : BSD
Maintainer  : alexg@chalmers.se
Stability   : experimental

Authors     : <list your names here>
Lab group   : <group number>
-}

module RunGame where

import Data.Char
import Test.QuickCheck
import Cards
import Data.Maybe(fromJust)

-- The interface to the students' implementation.
data Interface = Interface
  { iFullDeck :: Deck
  , iValue    :: Hand -> Int
  , iDisplay  :: Hand -> String
  , iGameOver :: Hand -> Bool
  , iWinner   :: Hand -> Hand -> Player
  , iDraw     :: Deck -> Hand -> (Deck, Hand)
  , iPlayBank :: Deck -> Hand
  , iShuffle  :: [Double] -> Deck -> Deck
  }

-- A type of players.
data Player = Guest | Bank deriving (Show, Eq)

-- Runs a game given an implementation of the interface.
runGame :: Interface -> IO ()
runGame i = do
  putStrLn "Welcome to the game."
  deck <- shuffleCards i
  gameLoop i deck []

shuffleCards :: Interface -> IO Deck
shuffleCards i = do
  Rand ds <- generate arbitrary
  return $ iShuffle i ds $ iFullDeck i

-- Play until the guest player is bust or chooses to stop.
gameLoop :: Interface -> Deck -> Hand -> IO ()
gameLoop i deck guest = do
  putStrLn $ "Your current score: " ++ displayHand i guest ++ "\n"
  if iGameOver i guest 
    then finish i deck guest
    else do
      draw <- ask guest
      if draw
        then let (deck', guest') = iDraw i deck guest in gameLoop i deck' guest'
        else finish i deck guest

ask :: Hand -> IO Bool
ask guest = do
  putStr $ "Draw " ++ (if null guest then "a " else "another ") ++ "card? [y] "
  yn <- getLine
  return $ null yn || map toLower yn == "y"

-- Display the bank's final score and the winner.
finish :: Interface -> Deck -> Hand -> IO ()
finish i deck guest = do
  putStrLn $ "Your final score: " ++ displayHand i guest
  putStrLn $ "The bank's final score: " ++ displayHand i bank
  putStrLn $ "Winner: " ++ show (iWinner i guest bank)
 where
  bank = iPlayBank i deck

-- A helper function for displaying a hand
displayHand :: Interface -> Hand -> String
displayHand i hand = show (iValue i hand) ++ cards
 where
   cards = if null hand then "" else " with cards: " ++ iDisplay i hand

