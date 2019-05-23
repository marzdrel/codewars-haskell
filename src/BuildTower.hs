module BuildTower where

buildTower :: Int -> [String]
buildTower floor = 
  map (buildFloor (last steps)) steps 
  where 
    steps = map (\x -> x * 2 - 1) (take floor [1..])
    buildFloor max current = margin ++ stars ++ margin
      where
        margin = replicate (div (max - current) 2) ' '
        stars = replicate current '*'
