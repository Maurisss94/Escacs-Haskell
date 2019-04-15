module Tauler(
    Peca (..)
    ) where 

import Data.List

type Posicio = String
type Parell = [(Peca, Posicio)]

data Tauler = Tauler Parell


iniciaTauler :: (Eq a) => [a] -> [a] -> [a]
iniciaTauler a b = [ x++y | x<-a, y<-b ]