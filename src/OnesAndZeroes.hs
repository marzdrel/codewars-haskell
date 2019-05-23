module OnesAndZeroes (toNumber) where

toNumber :: [Int] -> Int
toNumber = foldl1 (\a b -> b + 2 * a) 
