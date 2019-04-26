module Tauler(
    Tauler(..),
    crearTauler
) where 

import Peca
import Posicio
import Data.List


type LlistaParell = [(Peca, Posicio)]

data Tauler = Tauler LlistaParell

instance Show Tauler where
    show (Tauler t) = "   " ++ replicate 10 '=' ++ "\n" ++
                      "8- |" ++ imprimirFila 8 t (reverse [1 .. 8]) ++ "|" ++ "\n" ++
                      "7- |" ++ imprimirFila 7 t (reverse [1 .. 8]) ++ "|" ++ "\n" ++
                      "6- |" ++ imprimirFila 6 t (reverse [1 .. 8]) ++ "|" ++ "\n" ++
                      "5- |" ++ imprimirFila 5 t (reverse [1 .. 8]) ++ "|" ++ "\n" ++
                      "4- |" ++ imprimirFila 4 t (reverse [1 .. 8]) ++ "|" ++ "\n" ++
                      "3- |" ++ imprimirFila 3 t (reverse [1 .. 8]) ++ "|" ++ "\n" ++
                      "2- |" ++ imprimirFila 2 t (reverse [1 .. 8]) ++ "|" ++ "\n" ++
                      "1- |" ++ imprimirFila 1 t (reverse [1 .. 8]) ++ "|" ++ "\n" ++
                      "   " ++ replicate 10 '=' ++ "\n" ++ "    abcdefgh \n" 


                    
imprimirFila :: Int -> LlistaParell -> [Int] -> String
imprimirFila n xs (i:is) | length is == 0 = show (iBuscarPeca (n, i) xs)
imprimirFila n xs (i:is) = imprimirFila n xs is ++ show (iBuscarPeca (n, i) xs)                      





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
iBuscarPeca pos llista |  not (correcte pos) = error "La posició especificada no es troba dins del taulell"
iBuscarPeca pos (x:xs) | snd x == pos = fst x
iBuscarPeca pos (x:xs) = iBuscarPeca pos xs

moviment :: Peca -> Posicio -> [Posicio]
moviment _ pos | not (correcte pos) = error "La posició especificada no es troba dins del taulell"
moviment peca pos =
    if (tipus peca == Torre) then movimentsTorre pos
    else if (tipus peca == Cavall) then movimentsCavall pos
    else if (tipus peca == Alfil) then movimentsAlfil pos
    else if (tipus peca == Dama) then movimentsDama pos
    else if (tipus peca == Rei) then movimentsRei pos
    else
        if ((color peca) == (Negre))
        then movimentsPeo pos (color peca)
        else movimentsPeo pos (color peca)
--moviment _ _ = error "aaaa!";


