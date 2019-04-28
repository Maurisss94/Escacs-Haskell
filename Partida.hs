module Partida (
    Partida(..),
    Tauler(..),
    crearTauler,
    jugadaLegal
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

-- Comprova que, la peça i la posicio de la jugada, concordi amb la disposicio del Tauler,
-- tambe comprova que la posicio desti, estigui dintre els moviments possibles de la peça
-- si tot això es correcte, comprova que la posicio desti estigui Buida
-- per ultim, comprova que no hi hagi algu entre la posicio origen i desti.
-- Si tot aixo es cert, retorna jugadaLegal -> True
jugadaLegal :: Jugada -> Tauler -> Bool
jugadaLegal j tauler = 
    let pecaMoviment = tipusPeca j
        movimentsPeca = if pecaMoviment == (buscarPeca (origen j) tauler) 
            then moviment pecaMoviment (origen j) else []
        destiPeca = ((desti j) `elem` movimentsPeca)
        esLliure = destiPeca && ((buscarPeca (desti j) tauler) == Buida)
        esLegal = if (tipus pecaMoviment /= Cavall) 
            && (tipus pecaMoviment /= Rei) 
            && ((tipus pecaMoviment /= Peo) && ((snd (origen j) /= 2) || (snd (desti j) /= 7)))
            then esLliure && not (alguEntre tauler (origen j) (desti j))
            else esLliure

    in esLegal

-- Implementar metode guanyador, que donat una Partida et retorna, no se com, el torn del guanyador