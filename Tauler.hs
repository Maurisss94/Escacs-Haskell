import Peca

import Data.List

type Posicio = String
type LlistaParell = [(Maybe Peca, Posicio)]

data Tauler = Tauler LlistaParell deriving (Show)

obtenirLlistaTauler :: Tauler -> LlistaParell
obtenirLlistaTauler (Tauler t) = t

generarPosicions :: [String] -> [String] -> [String]
generarPosicions a b = [ x++y | y<-b, x<-a ]

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
crearTauler :: String -> Tauler
crearTauler [] = Tauler []
crearTauler xs = Tauler (crearLlistaParells taulerInicial (generarPosicions ["a","b","c","d","e","f","g","h"] ["8","7","6","5","4","3","2","1"]))

crearLlistaParells :: String -> [String] -> LlistaParell
crearLlistaParells "" []  = []
crearLlistaParells (x:xs) y | x == '\n' = crearLlistaParells xs y
crearLlistaParells (x:xs) (y:ys) | x == '.' = [(Nothing, y)]  ++  crearLlistaParells xs ys
crearLlistaParells (x:xs) (y:ys) =  [(Just (llegirPeca x), y)] ++ crearLlistaParells xs ys

obtenirValorCasella :: Posicio -> Tauler -> Maybe Peca
obtenirValorCasella p t = buscarPeca p (obtenirLlistaTauler t)


buscarPeca :: Posicio -> LlistaParell -> Maybe Peca
buscarPeca p ll =  fst . head $ filter ((( == ) p) . snd ) ll