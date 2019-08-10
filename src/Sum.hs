module Sum where
import Prelude hiding (sum)

sum :: Num a => [a] -> a
sum = sumN 0
  where
    sumN n [] = n
    sumN n (x:xs) = sumN (n + x) (xs)
