module Sumdivsq where

listSquared :: Int -> Int -> [(Int, Int)]
listSquared m n =
  filter (isSquared) $ map (squaredDivisors) [m..n]
  where
    isSquared (x, n) = floor (sqInt n) == ceiling (sqInt n)
    sqInt n = sqrt $ fromIntegral n::Double

getDivisorsFor i n acc
  | i <= div n 2 && rem n i == 0 = getDivisorsFor (i + 1) n (i:acc)
  | i <= div n 2 = getDivisorsFor (i + 1) n acc
  | otherwise = (n:acc)

squaredDivisors :: Int -> (Int, Int)
squaredDivisors n =
  (n, foldr ((+) . (^2)) 0 $ getDivisorsFor 1 n [])
