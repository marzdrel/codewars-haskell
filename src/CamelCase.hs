module CamelCase where

import qualified Data.List.Split as S
import qualified Data.Char as C

toCamelCase :: String -> String
toCamelCase src = 
  concat $ head list : rest
  where
    list = S.splitOneOf "-_" src
    rest = map (\(x:xs) -> C.toUpper x : xs) $ tail list
