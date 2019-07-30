module JosephusPermutation where

josephus :: [a] -> Int -> [a]
josephus list k =
  cycle [] list k
  where
    cycle acc [] k = reverse acc
    cycle acc list k = cycle (x:acc) xs k
      where
        (x:xs) = last $ take k $ iterate (\(x:xs) -> xs ++ [x]) list

