module MexicanWave where

import qualified Data.Char as Char

wave :: String -> [String]
wave input = wave' input [] []

wave' :: String -> String -> [String] -> [String]
wave' [] prefix current = current
wave' (' ':xs) prefix current = wave' xs (prefix ++ [' ']) current
wave' (x:xs) prefix current =
  wave' xs (prefix ++ [x]) (current ++ [prefix ++ [Char.toUpper x] ++ xs])

