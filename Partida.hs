module Partida (
    Partida(..),
    Tauler(..),
    crearTauler
) where

import Tauler
import Jugada
import Peca
import Posicio

type Torn = Bool
data Partida = Partida Tauler Torn deriving (Show)

tauler :: Partida -> Tauler
tauler (Partida tauler _) = tauler

torn :: Partida -> Torn
torn (Partida _ torn) = torn

--fesJugada :: Tauler -> Jugada -> Tauler

jugadaLegal :: Jugada -> Tauler -> Posicio
jugadaLegal j tauler = (1,2)

-- Implementar metode guanyador, que donat una Partida et retorna, no se com, el torn del guanyador


