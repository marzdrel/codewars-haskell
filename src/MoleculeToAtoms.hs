module MoleculeToAtoms where

import Debug.Trace

parseMolecule :: String -> Either String [(String, Int)]
parseMolecule formula =
  Right $ parseFormula (reverse formula) [] [] 1
  where
    parseFormula [] acc res mult = res
    parseFormula (x:xs) acc res mult
      | elem x ['A'..'Z'] =
        parseFormula xs [] (addDict (x:acc) mult res) 1
      | elem x ['a'..'z'] =
        parseFormula xs (x:acc) res mult
      | elem x ['0'..'9'] =
        parseFormula xs [] res (read [x]::Int)

addDict key value assoc =
  (key, nextValue):(filter ((key /=) . fst) assoc)
  where
    fetchDict ad Nothing = ad + 0
    fetchDict ad (Just n) = ad + n
    nextValue = fetchDict value $ lookup key assoc
