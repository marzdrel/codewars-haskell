{-
A generalization of Bézier surfaces, called the S-patch, uses an
interesting scheme for indexing its control points. In the case of an
n-sided surface of degree d, each index has n non-negative integers that
sum to d, and all possible configurations are used.

For example, for a 3-sided quadratic (degree 2) surface the control
points are:

indices 3 2 => [[0,0,2],[0,1,1],[0,2,0],[1,0,1],[1,1,0],[2,0,0]]

Given the degree and the number of sides, generate all control point
indices. The order of the indices in the list can be arbitrary, so for
the above example

[[1,1,0],[2,0,0],[0,0,2],[0,2,0],[0,1,1],[1,0,1]]

is also a good solution.
-}

module IntegerPartitions where

indices :: Int -> Int -> [[Int]]
indices 1 0 = [[0]]
indices n d =
  last $ take (n - 1) $ iterate (concatMap (reFlow)) (splitInt d)
  where
    reFlow n = map (\x -> (init n) ++ x) (splitInt (last n))
    splitInt n = map (\x -> [x, n - x]) [0..n]
