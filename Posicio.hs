module Posicio (
    Posicio(..),
    correcte,
    caracterAEnter)
where

import Data.Char(isDigit)


type Posicio = (Int, Int)

-- Mètode per comprovar si una posició es vàlida (diu si la posicio es troba dins d'un tauler de 8x8)
correcte :: Posicio -> Bool
correcte (x, y) = correcte x && correcte y
   where correcte x = x >= 1 && x <= 8

caracterAEnter :: Char -> Int
caracterAEnter c = if isDigit c then buscar c (zip ['1'..'8'] [1..8]) else buscar c (zip ['a'..'h'] [1..8])

buscar :: (Eq a) => a -> [(a,b)] -> b
buscar _ [] =  error "L'element no existeix"
buscar c ((a,b):xs) = if c == a then b else buscar c xs