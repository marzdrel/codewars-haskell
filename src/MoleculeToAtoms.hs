module MoleculeToAtoms where

import Debug.Trace

parseMolecule :: String -> Either String [(String, Int)]
parseMolecule formula =
  Right $ parseFormula formula [] []
  where
    parseFormula [] acc res
      | null acc = res
      | otherwise = addDict (last acc) 1 res
    parseFormula (x:xs) (acc) res
      | elem x ['A'..'Z'] && null acc =
        parseFormula xs [[x]] res
      | elem x ['A'..'Z'] =
        parseFormula xs [[x]] $
        parseFormula xs [] (addDict (last acc) 1 res)
      | elem x ['a'..'z'] =
        parseFormula xs (init acc ++ [last acc ++ [x]]) res
      | elem x ['0'..'9'] =
        parseFormula xs [] (addDict (last acc) (read [x]::Int) res)
      | otherwise = res

addDict key value assoc =
  (key, nextValue):(filter ((key /=) . fst) assoc)
  where
    fetchDict ad Nothing = ad + 0
    fetchDict ad (Just n) = ad + n
    nextValue = fetchDict value $ lookup key assoc
