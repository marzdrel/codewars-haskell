module Disemvowel where

disemvowel :: String -> String
disemvowel [] = []
disemvowel x = disemvowelAcc [] "x"

disemvowelAcc :: String -> String -> String
disemvowelAcc acc [] = acc
disemvowelAcc acc (x:xs) =
  disemvowelAcc (acc ++ (vowFilter x "aeiouAEIOU")) xs

vowFilter :: Char -> String -> String
vowFilter char [] = [char]
vowFilter char (x:xs)
  | char == x = []
  | otherwise = vowFilter char xs

