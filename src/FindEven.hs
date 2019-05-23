module FindEven where

import Control.Applicative ((<$>), (<*>))
import Data.List (elemIndex)
import Data.Maybe (fromMaybe)

findEvenIndex :: [Int] -> Int
findEvenIndex arr = compareList 0 0 arr
  where 
    compareList left nstep [] = -1
    compareList left nstep (x:xs)
      | left == sum (xs) = nstep
      | otherwise = compareList (left + x) (nstep + 1) xs

findEvenIndex2 :: [Int] -> Int
findEvenIndex2 = 
  fromMaybe (-1) . 
  elemIndex True .
  (zipWith (==) <$> scanl1 (+) <*> scanr1 (+))
