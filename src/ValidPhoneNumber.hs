module ValidPhoneNumber where

validPhoneNumber :: String -> Bool
validPhoneNumber number
  | ldiff number /= 0 = False
  | otherwise = checkStringAt number
  where
    numberFormat = "(ddd) ddd-dddd"
    ldiff string = abs $ length string - length numberFormat
    compareStr pattern char
      | pattern == 'd' = elem char ['0'..'9']
      | otherwise = pattern == char
    checkStringAt [] = True
    checkStringAt (x:xs)
      | compareStr (numberFormat !! (ldiff (x:xs))) x = checkStringAt xs
      | otherwise = False
