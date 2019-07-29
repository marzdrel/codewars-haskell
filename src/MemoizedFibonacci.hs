module MemoizedFibonacci where

fibonacci :: Int -> Int
fibonacci = (map memoFib [0..] !!)
  where
    memoFib 0 = 0
    memoFib 1 = 1
    memoFib n = fibonacci (n-1) + fibonacci (n-2)
