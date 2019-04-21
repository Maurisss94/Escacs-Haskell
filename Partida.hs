import Tauler

type Torn = Bool
data Partida = Partida Tauler Torn deriving (Eq, Show)

tauler :: Partida -> Tauler
tauler (Partida tauler _) = tauler

torn :: Partida -> Torn
torn (Partida _ torn) = torn

-- Implementar metode guanyador, que donat una Partida et retorna, no se com, el torn del guanyador