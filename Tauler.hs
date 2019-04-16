import Peca

import Data.List

type Posicio = String
type Parell = [(Peca, Posicio)]

data Tauler = Tauler Parell deriving (Show)


--generarPosicions :: Tauler -> [String] -> [String]
--generarPosicions a b = [ x++y | x<-a, y<-b ]