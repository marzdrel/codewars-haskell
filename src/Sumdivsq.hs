module Sumdivsq where

listSquared :: Int -> Int -> [(Int, Int)]
listSquared m n =
  filter (isSquared) $ map (squaredDivisors) [m..n]
  where
    isSquared (_, n) = (isInt . sqrt . fromIntegral) n
    isInt x = x == fromInteger (round x)

divisors :: Int -> [Int]
divisors n = filter ((0 ==) . (mod n)) [1 .. (div n 2)]

squaredDivisors :: Int -> (Int, Int)
squaredDivisors n =
  (n, foldr ((+) . (^2)) 0 $ (n:divisors n))
