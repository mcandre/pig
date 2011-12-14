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

-- Assumes scores start below 100 and run is empty

play :: [Player] -> Int -> Run -> IO ()
play (p:ps) t r = do
	let n = name p
	let s = strategy p
	let m = s (p:ps) r

	case m of
		Hold -> do
			say t n "holds."

			let score' = score p + sum r

			say t n $ "has " ++ show score' ++ " total points."

			if score' >= 100 then
				say t n "wins!"
			else do
				let p' = p { score = score' }
				let ps' = ps ++ [p']
				play ps' (t+1) []
		Roll -> do
			pips <- roll
			say t n ("rolled " ++ show pips ++ ".")

			if pips == 1 then do
				say t n "pigged."
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

rollOnce :: Strategy
rollOnce _ [] = Roll
rollOnce _ (r:_) = Hold

roll5 :: Strategy
roll5 (p:_) rs
	| score p + sum rs >= 100 = Hold
	| length rs < 5 = Roll
	| otherwise = Hold

rollK :: Strategy
rollK (p:ps) rs
	| score p + sum rs >= 100 = Hold
	| winning && (length rs < 4) = Roll
	| length rs < 6 = Roll
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
ro = defaultPlayer { name = "Roll Once", strategy = rollOnce }
rf = defaultPlayer { name = "Roll Five", strategy = roll5 }
rk = defaultPlayer { name = "Roll K", strategy = rollK }

main :: IO ()
main = do
	let ps = [nr, ar, ro, rf, rk]
	ps' <- runRVar (shuffle ps) DevRandom
	play ps' 1 []