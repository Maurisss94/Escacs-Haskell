module Posicio (
    Posicio(..),
    correcte) where

type Posicio = (Int, Int)

-- Mètode per comprovar si una posició es vàlida (diu si la posicio es troba dins d'un tauler de 8x8)
correcte :: Posicio -> Bool
correcte (x, y) = correcte x && correcte y
   where correcte x = x >= 1 && x <= 8