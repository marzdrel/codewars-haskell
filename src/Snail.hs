module Snail where

import Debug.Trace

snail :: [[Int]] -> [Int]
snail = snail' []

snail' :: [Int] -> [[Int]] -> [Int]
snail' acc (x:xs)
  | trace (show x) $ null x = acc
  | otherwise = snail' (acc ++ (outerPart (x:xs))) (innerPart xs)
  where
    innerPart x
      | null $ tail x = []
      | otherwise = trace (show x) $ map (tail . init) x
    outerPart (x:xs) = trace (show x) $ []
      {- x ++ (map (last) (init xs)) ++ (reverse $ last xs) ++ (map (head) (init xs)) -}

{- snail [[1,2,3,4],[4,5,6,1],[7,8,9,2],[1,2,3,4]] -}
