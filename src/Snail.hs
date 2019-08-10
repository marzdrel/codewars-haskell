module Snail where

import Data.List

snail :: [[Int]] -> [Int]
snail [] = []
snail (xs:xss) = xs ++ (snail . reverse . transpose) xss

snail2 :: [[Int]] -> [Int]
snail2 = snail2' []
snail2' :: [Int] -> [[Int]] -> [Int]
snail2' acc [] = acc
snail2' acc (x:xs) = snail2' (acc ++ (outerPart (x:xs))) (innerPart xs)
  where
    innerPart [] = []
    innerPart x = map (tail . init) (init x)
    outerPart [] = []
    outerPart (x:[]) = x
    outerPart (x:xs) =
      x ++
      (map (last) (init xs)) ++
      (reverse $ last ([]:xs)) ++
      (reverse $ (map (head) (init xs)))
