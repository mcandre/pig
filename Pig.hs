#!/usr/bin/env runhaskell

-- Andrew Pennebaker
-- 13 Dec 2011

{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Pig where

import Prelude hiding (lookup)

import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Map hiding (null, filter)
import Data.Maybe (fromMaybe)

import Data.Random
import Data.Random.Source.DevRandom
import Data.Random.Extras hiding (shuffle)

import Control.Monad (replicateM)

roll :: IO Int
roll = runRVar (choice [1..6]) DevRandom

data Move = Roll | Hold

type Strategy = [Player] -> [Int] -> Move

data Player = Player {
  name :: String,
  strategy :: Strategy,
  score :: Int
  }

sayN :: Int -> Int -> String -> String -> IO ()
sayN _ _ _ _ = return ()
-- sayN playerCount turn name message = putStrLn $ "[Round " ++ show (turn `div` playerCount) ++ "] " ++ name ++ " " ++ message

-- Play a game of Pig and return the winner
play :: [Player] -> Int -> [Int] -> IO Player
play [] _ _ = return Player { name = "", strategy = alwaysHold, score = 0 }
play (p:ps) t r = do
  let n = name p
  let s = strategy p
  let m = s (p:ps) r

  case m of
    Hold -> do
      say t n "holds."

      let score' = score p + sum r
      let p' = p { score = score' }

      say t n $ "has " ++ show score' ++ " total points."

      if score' >= 100 then do
        say t n "wins!"
        return p'
        else do
          let ps' = ps ++ [p']
          play ps' (t+1) []
    Roll -> do
      pips <- roll
      say t n ("rolled " ++ show pips ++ ".")

      if pips == 1 then do
          say t n "pigged."
          say t n $ "has " ++ show (score p) ++ " total points."

          let ps' = ps ++ [p]
          play ps' (t+1) []
        else do
          let r' = r ++ [pips]
          play (p:ps) t r'
  where
    say = sayN (length ps + 1)

alwaysHold :: Strategy
alwaysHold _ _ = Hold

alwaysRoll :: Strategy
alwaysRoll _ _ = Roll

hundredOrBust :: Strategy
hundredOrBust [] _ = Hold
hundredOrBust (p:_) rs
  | score p + sum rs >= 100 = Hold
  | otherwise = Roll

rollOnce :: Strategy
rollOnce _ [] = Roll
rollOnce _ _ = Hold

roll5 :: Strategy
roll5 [] _ = Hold
roll5 (p:_) rs
  | score p + sum rs >= 100 = Hold
  | length rs < 5 = Roll
  | otherwise = Hold

roll6 :: Strategy
roll6 [] _ = Hold
roll6 (p:_) rs
  | score p + sum rs >= 100 = Hold
  | length rs < 6 = Roll
  | otherwise = Hold

rollK :: Strategy
rollK [] _ = Hold
rollK (p:ps) rs
  | score p + sum rs >= 100 = Hold
  | winning && (length rs < 2) = Roll
  | length rs < 6 = Roll
  | otherwise = Hold
  where
    challengers = sortBy (comparing score) (p:ps)
    winning = name (last challengers) == name p

rollBadK :: Strategy
rollBadK [] _ = Hold
rollBadK (p:ps) rs
  | score p + sum rs >= 100 = Hold
  | winning && (length rs < 6) = Roll
  | length rs < 2 = Roll
  | otherwise = Hold
  where
    challengers = sortBy (comparing score) (p:ps)
    winning = name (last challengers) == name p

defaultPlayer :: Player
defaultPlayer = Player {
  name = "Player",
  strategy = roll5,
  score = 0
  }

ah :: Player
ah = defaultPlayer { name = "Always Hold", strategy = alwaysHold }

ar :: Player
ar = defaultPlayer { name = "Always Roll", strategy = alwaysRoll }

hob :: Player
hob = defaultPlayer { name = "100 or Bust", strategy = hundredOrBust }

ro :: Player
ro = defaultPlayer { name = "Roll Once", strategy = rollOnce }

r5 :: Player
r5 = defaultPlayer { name = "Roll Five", strategy = roll5 }

r6 :: Player
r6 = defaultPlayer { name = "Roll Six", strategy = roll6 }

rk :: Player
rk = defaultPlayer { name = "Roll K Times", strategy = rollK }

rb :: Player
rb = defaultPlayer { name = "Roll Bad K", strategy = rollBadK }

test :: [Player] -> IO Player
test ps = do
  ps' <- runRVar (shuffle ps) DevRandom
  play ps' 1 []

track :: [Player] -> Map String Int -> Map String Int
track [] m = m
track (p:ps) m = track ps m'
  where
    n = name p
    wins = fromMaybe 0 (lookup n m)
    m' = insert n (wins + 1) m

stats :: [Player] -> [(String, Int)]
stats = sortBy (flip (comparing snd)) . toList . flip track empty

addLosers :: [Player] -> [(String, Int)] -> [(String, Int)]
addLosers [] results = results
addLosers (p:ps) results
  | not (any (\(n, _) -> n == name p) results) = addLosers ps $ results ++ [(name p, 0)]
  | otherwise = addLosers ps results

main :: IO ()
main = do
  let ps = [ah, ar, hob, ro, r5, r6, rk, rb]
  let n = 10000

  putStrLn $ "Running " ++ show n ++ " games..."

  winners <- replicateM n (test ps)

  putStrLn "Totaling wins...\n"

  let winners' = stats winners
  let winners'' = addLosers ps winners'

  mapM_ (\(name', wins) -> putStrLn $ name' ++ "\t" ++ show (100 * wins `div` n) ++ "%") winners''