module Posicio (
    Posicio(..),
    correcte,
    caracterAEnter)
where


-- Importació de llibreries adicionals.
import Data.Char(isDigit)



-- Definició del tipus 'Posicio'
type Posicio = (Int, Int)



-- Funció per comprovar si una posició es vàlida (diu si la posicio es troba dins d'un tauler de 8x8).
-- * Paràmetre 1: Posició sobre la qual es vol consultar si es troba dins del tauler o no.
-- ** Retorn: 'True' si la posició es troba dins d'un tauler de 8x8, altrament retorna 'False'.
correcte :: Posicio -> Bool
correcte (x, y) = correcte x && correcte y
   where correcte x = x >= 1 && x <= 8

-- Funció per fer la conversió de caràcter a número enter.
-- * Paràmetre 1: Caràcter a partir del qual es vol obtenir el valor enter equivalent.
-- ** Retorn: Valor enter corresponent al caràcter rebut per paràmetre.  
caracterAEnter :: Char -> Int
caracterAEnter c = if isDigit c then buscar c (zip ['1'..'8'] [1..8]) else buscar c (zip ['a'..'h'] [1..8])

--Funció per cercar un element dins d'una llista de tuples i retornar el segon element de la tupla.
-- * Paràmetre 1: Valor del primer element de la tupla a cercar.
-- ** Retorn: En el cas de que es trobi el valor pasat per paràmetre es retorna el segon element de la tupla amb primer element coincident amb el paràmetre, altrament retorna error.
buscar :: (Eq a) => a -> [(a,b)] -> b
buscar _ [] =  error "L'element no existeix"
buscar c ((a,b):xs) = if c == a then b else buscar c xs