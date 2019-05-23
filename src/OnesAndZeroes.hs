module OnesAndZeroes (toNumber) where

toNumber :: [Int] -> Int
toNumber = foldl1 ((+) . (* 2))
