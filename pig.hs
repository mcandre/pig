#!/usr/bin/env runhaskell

-- Andrew Pennebaker
-- 13 Dec 2011

{-# LANGUAGE TypeSynonymInstances #-}

import Data.List (sortBy)

import Data.Random
import Data.Random.Source.DevRandom
import Data.Random.Extras hiding (shuffle)

roll :: IO Int
roll = runRVar (choice [1..6]) DevRandom

type Score = Int

type Run = [Score]

data Move = Roll | Hold

type Strategy = [Player] -> Run -> Move

instance Eq Strategy where
	(==) a b = True

data Player = Player {
		name :: String,
		strategy :: Strategy,
		score :: Score
	} deriving (Eq)

-- data Game = Game [Player]

sayN :: Int -> Int -> String -> String -> IO ()
sayN playerCount turn name message = putStrLn $ "[Round " ++ show (turn `div` playerCount) ++ "] " ++ name ++ " " ++ message

-- Play a game of Pig and return the winner

play :: [Player] -> Int -> Run -> IO ()
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

neverRoll :: Strategy
neverRoll _ _ = Hold

alwaysRoll :: Strategy
alwaysRoll _ _ = Roll

rollUntil100 :: Strategy
rollUntil100 (p:ps) rs
	| score p + sum rs >= 100 = Hold
	| otherwise = Roll

rollOnce :: Strategy
rollOnce _ [] = Roll
rollOnce _ (r:_) = Hold

roll5 :: Strategy
roll5 (p:_) rs
	| score p + sum rs >= 100 = Hold
	| length rs < 5 = Roll
	| otherwise = Hold

roll6 :: Strategy
roll6 (p:_) rs
	| score p + sum rs >= 100 = Hold
	| length rs < 6 = Roll
	| otherwise = Hold

rollK :: Strategy
rollK (p:ps) rs
	| score p + sum rs >= 100 = Hold
	| winning && (length rs < 2) = Roll
	| length rs < 5 = Roll
	| otherwise = Hold
	where
		challengers = (sortBy (\a b -> compare (score a) (score b))) (p:ps)
		winning = last challengers == p

rollBadK :: Strategy
rollBadK (p:ps) rs
	| score p + sum rs >= 100 = Hold
	| winning && (length rs < 5) = Roll
	| length rs < 2 = Roll
	| otherwise = Hold
	where
		challengers = (sortBy (\a b -> compare (score a) (score b))) (p:ps)
		winning = last challengers == p

defaultPlayer :: Player
defaultPlayer = Player {
		name = "Player",
		strategy = roll5,
		score = 0
	}

nr = defaultPlayer { name = "Never Roll", strategy = neverRoll }
ar = defaultPlayer { name = "Always Roll", strategy = alwaysRoll }
ru = defaultPlayer { name = "Roll Until 100", strategy = rollUntil100 }
ro = defaultPlayer { name = "Roll Once", strategy = rollOnce }
r5 = defaultPlayer { name = "Roll Five", strategy = roll5 }
r6 = defaultPlayer { name = "Roll Six", strategy = roll6 }
rk = defaultPlayer { name = "Roll K", strategy = rollK }
rb = defaultPlayer { name = "Roll Bad K", strategy = rollBadK }

test :: [Player] -> IO ()
test ps = do
	ps' <- runRVar (shuffle ps) DevRandom
	play ps' 1 []

main :: IO ()
main = do
	let ps = [nr, ar, ru, ro, r5, r6, rk, rb]
	test ps