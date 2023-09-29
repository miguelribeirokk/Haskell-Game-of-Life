import Data.List (lookup)
import Data.Maybe (fromMaybe)

-- Indice indiceLetra 'B'
-- Tuplas: [('A', 1), ('B', 2), ('C', 3), ...]
indiceLetra :: Char -> Int
indiceLetra c = fromMaybe 0 m
    {-
    case m of 
        Nothing -> 0
        Just i -> i
    -}
    where m = lookup c (zip ['A'..'Z'][1..])