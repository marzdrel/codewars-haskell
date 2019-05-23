module FindEven where

findEvenIndex :: [Int] -> Int
findEvenIndex arr = compareList 0 0 arr
  where 
    compareList left nstep [] = -1
    compareList left nstep (x:xs)
      | left == sum (xs) = nstep
      | otherwise = compareList (left + x) (nstep + 1) xs
