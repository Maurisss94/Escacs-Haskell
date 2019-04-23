module Partida (
    Partida(..),
    Tauler(..),
    Jugada(..),
    crearTauler
) where

import Tauler
import Jugada

type Torn = Bool
data Partida = Partida Tauler Torn Jugada deriving (Show)

tauler :: Partida -> Tauler
tauler (Partida tauler _ _) = tauler

torn :: Partida -> Torn
torn (Partida _ torn _) = torn

jugada :: Partida -> Jugada
jugada (Partida _ _ jugada) = jugada

-- Implementar metode guanyador, que donat una Partida et retorna, no se com, el torn del guanyador