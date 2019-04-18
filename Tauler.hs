import Peca

import Data.List

type Posicio = (Int, Int)
type LlistaParell = [(Peca, Posicio)]

data Tauler = Tauler LlistaParell deriving (Show)


obtenirLlistaTauler :: Tauler -> LlistaParell
obtenirLlistaTauler (Tauler t) = t

generarPosicions :: [Int] -> [Int] -> [Posicio]
generarPosicions a b = [ (x, y) | x<-a, y<-b ]

taulerInicial :: String
taulerInicial = intercalate "\n" ["tcadract", 
                                  "pppppppp", 
                                  "........",
                                  "........",
                                  "........",
                                  "........",
                                  "PPPPPPPP",
                                  "TCADRACT"] ++ "\n"

-- Mirar de simplificar les crides de generarPosicions, fer les llistes mes petites del estil [a..h], [8..1]
-- Param 1: String que representa l'estat inicial d'un tauler d'escacs.
-- Return: Retorna un tipus Tauler amb les posicions i peces.
crearTauler :: String -> Tauler
crearTauler [] = Tauler []
crearTauler xs = Tauler (inicialitzarValorsTauler taulerInicial (generarPosicions (reverse [1 .. 8]) [1 .. 8]))

-- Inicialitza els valors del tauler amb peces i posicions
-- Param
inicialitzarValorsTauler :: String -> [Posicio] -> LlistaParell
inicialitzarValorsTauler "" []  = []
inicialitzarValorsTauler (x:xs) y | x == '\n' = inicialitzarValorsTauler xs y
inicialitzarValorsTauler (x:xs) (y:ys) | x == '.' =  [] ++ inicialitzarValorsTauler xs ys
inicialitzarValorsTauler (x:xs) (y:ys) =  [((llegirPeca x), y)] ++ inicialitzarValorsTauler xs ys

--
--obtenirValorCasella :: Posicio -> Tauler -> Maybe Peca
--obtenirValorCasella p t = buscarPeca p (Tauler t)


--buscarPeca :: Posicio -> LlistaParell -> Maybe Peca
--buscarPeca p ll =  fst . head $ filter ((( == ) p) . snd ) ll