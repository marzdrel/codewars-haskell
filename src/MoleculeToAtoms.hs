module MoleculeToAtoms where

import Debug.Trace

debugGuard = (flip trace) False

parseMolecule :: String -> Either String [(String, Int)]
parseMolecule formula =
  parseBracketsResult $ parseBrackets formula [] []
  where
    parseBrackets :: String -> String -> String -> Either String String
    parseBrackets [] [] str = Right (reverse str)
    parseBrackets [] stack str = Left "error"
    parseBrackets (x:xs) stack str
      | elem x "[({" = parseBrackets xs (x:stack) (x:str)
      | checkStack x '[' ']' stack = Left "error"
      | checkStack x '(' ')' stack = Left "error"
      | checkStack x '{' '}' stack = Left "error"
      | elem x "])}" = parseBrackets xs (tail stack) (x:str)
      | otherwise = parseBrackets xs stack (x:str)
      where
        checkStack e open close stack =
           e == close && (null stack || (head stack) /= open)

    parseBracketsResult (Left x) = Left x
    parseBracketsResult (Right x) = parseFormulaResult $ parseFormula (reverse x) [] [] [1]
    parseFormulaResult a
      | null a = Left "error"
      | otherwise = Right a

    parseFormulaN :: String -> String -> [(String, Int)] -> [Int] -> [(String, Int)]
    parseFormulaN (x:xs) acc res mult
      | elem x ['0'..'9'] = parseFormula xs [] res (replacedCounter:(tail mult))
      | otherwise = parseFormula (x:xs) acc res mult
        where
          replacedCounter = (head mult) + (10 * read [x]::Int)

    parseFormula :: String -> String -> [(String, Int)] -> [Int] -> [(String, Int)]
    parseFormula [] acc res mult = res
    parseFormula (x:xs) acc res mult
      | debugGuard ("DEBUG: " ++ [x] ++ " " ++ (show mult)) = undefined
      | elem x ['A'..'Z'] =
        parseFormula xs [] (addDict (x:acc) (product mult) res) (1:(tail mult))
      | elem x ['a'..'z'] && null acc =
        parseFormula xs [x] res mult
      | elem x ['a'..'z'] = []
      | elem x ['0'..'9'] =
        parseFormulaN xs [] res ((read [x]::Int):(tail mult))
      | elem x "[{(" =
        parseFormula xs [] res (1:(tail . tail) mult)
      | elem x "]})" =
        parseFormula xs [] res (1:mult)
      | otherwise = res

addDict key value assoc =
  (key, nextValue):(filter ((key /=) . fst) assoc)
  where
    fetchDict ad Nothing = ad + 0
    fetchDict ad (Just n) = ad + n
    nextValue = fetchDict value $ lookup key assoc
