module Stringifier where

stringy :: Int -> String
stringy n = map addDigit [1..n]
  where
    addDigit n
      | odd n = '1'
      | otherwise = '0'
