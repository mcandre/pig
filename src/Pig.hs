-- Andrew Pennebaker
-- 13 Dec 2011

{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Pig simulates a dice game.
module Pig where

import Prelude hiding (lookup)

import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Map
import Data.Maybe (fromMaybe)

import qualified System.Random as Random
import qualified System.Random.Shuffle as Shuffle

-- | roll simulates a six-sided die.
roll :: IO Int
roll = Random.getStdRandom $ Random.randomR (1, 6)

-- | Move models valid game actions.
data Move = Roll | Hold

-- | Strategy models player tactics.
type Strategy = [Player] -> [Int] -> Move

-- | Player models information available to combatants.
data Player = Player {
  name :: String,
  strategy :: Strategy,
  score :: Int
  }

-- | sayN provides a debugging hook during development.
sayN :: Int -> Int -> String -> String -> IO ()
sayN _ _ _ _ = return ()
-- sayN playerCount turn name message = putStrLn $ "[Round " ++ show (turn `div` playerCount) ++ "] " ++ name ++ " " ++ message

-- | play executes a game of Pig and returns the winner.
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

-- | alwaysHold models a simple player who always holds.
alwaysHold :: Strategy
alwaysHold _ _ = Hold

-- | alwaysRoll models a simple player who always rolls.
alwaysRoll :: Strategy
alwaysRoll _ _ = Roll

-- | hundredOrBust models a player who strives for 100 points.
hundredOrBust :: Strategy
hundredOrBust [] _ = Hold
hundredOrBust (p:_) rs
  | score p + sum rs >= 100 = Hold
  | otherwise = Roll

-- | rollOnce models a player who rolls only once.
rollOnce :: Strategy
rollOnce _ [] = Roll
rollOnce _ _ = Hold

-- | roll5 models a player who rolls up to five times.
roll5 :: Strategy
roll5 [] _ = Hold
roll5 (p:_) rs
  | score p + sum rs >= 100 = Hold
  | length rs < 5 = Roll
  | otherwise = Hold

-- | roll6 models a player who rolls up to six times.
roll6 :: Strategy
roll6 [] _ = Hold
roll6 (p:_) rs
  | score p + sum rs >= 100 = Hold
  | length rs < 6 = Roll
  | otherwise = Hold

-- | rollK rolls to keep up with the current top player.
rollK :: Strategy
rollK [] _ = Hold
rollK (p:ps) rs
  | score p + sum rs >= 100 = Hold
  | winning && (length rs < 2) = Roll
  | length rs < 6 = Roll
  | otherwise = Hold
  where
    challengers = sortOn score (p:ps)
    winning = name (last challengers) == name p

-- | rollBadK is a poor player
rollBadK :: Strategy
rollBadK [] _ = Hold
rollBadK (p:ps) rs
  | score p + sum rs >= 100 = Hold
  | winning && (length rs < 6) = Roll
  | length rs < 2 = Roll
  | otherwise = Hold
  where
    challengers = sortOn score (p:ps)
    winning = name (last challengers) == name p

-- | defaultPlayer constructs a new player.
defaultPlayer :: Player
defaultPlayer = Player {
  name = "Player",
  strategy = roll5,
  score = 0
  }

-- | ah always holds.
ah :: Player
ah = defaultPlayer { name = "Always Hold", strategy = alwaysHold }

-- | ar always rolls.
ar :: Player
ar = defaultPlayer { name = "Always Roll", strategy = alwaysRoll }

-- | hob busts at 100.
hob :: Player
hob = defaultPlayer { name = "100 or Bust", strategy = hundredOrBust }

-- | ro rolls once.
ro :: Player
ro = defaultPlayer { name = "Roll Once", strategy = rollOnce }

-- | r5 rolls five times.
r5 :: Player
r5 = defaultPlayer { name = "Roll Five", strategy = roll5 }

-- | r6 rolls six times.
r6 :: Player
r6 = defaultPlayer { name = "Roll Six", strategy = roll6 }

-- | rk rolls to keep up with the top player.
rk :: Player
rk = defaultPlayer { name = "Roll K Times", strategy = rollK }

-- | rb is a bad player.
rb :: Player
rb = defaultPlayer { name = "Roll Bad K", strategy = rollBadK }

-- | test executes multiple games.
test :: [Player] -> IO Player
test ps = do
  stdGen <- Random.getStdGen
  let ps' = Shuffle.shuffle' ps (length ps) stdGen
  play ps' 1 []

-- | track records player wins across games.
track :: [Player] -> Map String Int -> Map String Int
track [] m = m
track (p:ps) m = track ps m'
  where
    n = name p
    wins = fromMaybe 0 (lookup n m)
    m' = insert n (wins + 1) m

-- | stats sorts game scores.
stats :: [Player] -> [(String, Int)]
stats = sortBy (flip (comparing snd)) . toList . flip track empty

-- | addLosers identifies consistently low-scoring strategies.
addLosers :: [Player] -> [(String, Int)] -> [(String, Int)]
addLosers [] results = results
addLosers (p:ps) results
  | not (any (\(n, _) -> n == name p) results) = addLosers ps $ results ++ [(name p, 0)]
  | otherwise = addLosers ps results
