module Main where

import Control.Monad (replicateM)

import Pig

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
