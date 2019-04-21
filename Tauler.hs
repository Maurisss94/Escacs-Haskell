module Tauler(
    Tauler(..),
    crearTauler
) where 

import Peca
import Data.List

type Posicio = (Int, Int)
type LlistaParell = [(Peca, Posicio)]

data Tauler = Tauler LlistaParell deriving (Show)


obtenirLlistaTauler :: Tauler -> LlistaParell
obtenirLlistaTauler (Tauler t) = t

-- Metode que genera tota la combinatoria de Posicio de dues llistes
-- Param1: Llista d'enters
-- Param2: Llista d'enters
generarPosicions :: [Int] -> [Int] -> [Posicio]
generarPosicions a b = [ (x, y) | x<-a, y<-b ]

-- String que representa l'estat inicial del tauler d'escacs
taulerInicial :: String
taulerInicial = intercalate "\n" ["tcadract", 
                                  "pppppppp", 
                                  "........",
                                  "........",
                                  "........",
                                  "........",
                                  "PPPPPPPP",
                                  "TCADRACT"] ++ "\n"

-- Param 1: String que representa l'estat inicial d'un tauler d'escacs.
-- Return: Retorna un tipus Tauler amb les posicions i peces.
crearTauler :: Tauler
crearTauler = Tauler (inicialitzarValorsTauler taulerInicial (generarPosicions (reverse [1 .. 8]) [1 .. 8]))

-- Inicialitza els valors del tauler amb peces i posicions
-- Param 1: String de la disposicio del tauler ex: "ptpppctp\n........\nppp"
-- Param 2: Llista de Posicions necessaries per crear la llista de Parells [(Peca, Posicio)]
inicialitzarValorsTauler :: String -> [Posicio] -> LlistaParell
inicialitzarValorsTauler "" []  = []
inicialitzarValorsTauler (x:xs) y | x == '\n' = inicialitzarValorsTauler xs y
inicialitzarValorsTauler (x:xs) (y:ys) | x == '.' =  [] ++ inicialitzarValorsTauler xs ys
inicialitzarValorsTauler (x:xs) (y:ys) =  [((llegirPeca x), y)] ++ inicialitzarValorsTauler xs ys


buscarPeca :: Posicio -> Tauler -> Peca
buscarPeca pos (Tauler t) = iBuscarPeca pos t

iBuscarPeca :: Posicio -> LlistaParell -> Peca
iBuscarPeca pos [] = Buida
iBuscarPeca pos llista |  (fst pos > 8 || snd pos > 8 ) = error "fora tauler"
iBuscarPeca pos (x:xs) | snd x == pos = fst x
iBuscarPeca pos (x:xs) = iBuscarPeca pos xs
