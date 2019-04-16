import Peca

import Data.List

type Posicio = String
type LlistaParell = [(Peca, Posicio)]

data Tauler = Tauler LlistaParell deriving (Show)


--generarPosicions :: Tauler -> [String] -> [String]
--generarPosicions a b = [ x++y | x<-a, y<-b ]

taulerInicial :: String
taulerInicial = intercalate "\n" ["tcadract", 
                                  "pppppppp", 
                                  "........",
                                  "........",
                                  "........",
                                  "PPPPPPPP",
                                  "TCADRACT"] ++ "\n"

llegirTauler :: String -> Tauler
llegirTauler [] = Tauler []
llegirTauler (x:xs) = Tauler (crearPair xs)

crearPair :: String -> [(Peca, Posicio)]
crearPair "" = []
crearPair (x:xs) =  [(llegirPeca x, "a")] ++ crearPair lines xs

obtenirCaracter :: String -> Char
obtenirCaracter [] = '\n'
obtenirCaracter (x:xs) = x


remFirst :: String -> String
remFirst "" = ""
remFirst cs = tail cs