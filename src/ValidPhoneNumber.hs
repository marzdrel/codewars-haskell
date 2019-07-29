module ValidPhoneNumber where

validPhoneNumber :: String -> Bool
validPhoneNumber number =
  map (mapDigit) number == "(000) 000-0000"
  where
    mapDigit char
      | elem char ['0'..'9'] = '0'
      | otherwise = char

validPhoneNumber2 :: String -> Bool
validPhoneNumber2 number
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
