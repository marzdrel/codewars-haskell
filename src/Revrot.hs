module Revrot where

splitString :: Int -> [Char] -> [String]
splitString 0 _ = []
splitString _ [] = []
splitString n xs = head : splitString n tail
  where (head, tail) = splitAt n xs

processString :: [Int] -> [Int]
processString arr
  | rem (sum $ map (^3) arr) 2 == 0 = reverse arr
  | otherwise = xs ++ [x]
  where
    (x:xs) = arr

revRot :: [Char] -> Int -> [Char]
revRot strng sz =
  concatMap (concatMap show . processString) $
  map (map (\x -> read [x]::Int)) $
  filter ((== sz) . length) $
  splitString sz strng
