module Sumdivsq where

listSquared :: Int -> Int -> [(Int, Int)]
listSquared m n =
  filter (isSquared) $ map (squaredDivisors) [m .. n]
  where
    isSquared (_, n) = isInt . sqrt . fromIntegral $ n
    isInt x = x == fromInteger (round x)
    squaredDivisors n = (n, foldr ((+) . (^2)) 0 $ allDivisors n)
    divisors n = filter ((0 ==) . (mod n)) [1 .. floor . sqrt . fromIntegral $ n]
    allDivisors n = divisors n ++ [div n i | i <- divisors n, i * i /= n]
