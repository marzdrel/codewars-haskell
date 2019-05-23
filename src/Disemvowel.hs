module Disemvowel where

import Data.Char

disemvowel :: String -> String
disemvowel = filter (flip notElem "aeiou" . toLower)
