module Partida (
    Partida(..),
    Tauler(..),
    crearTauler,
    jugadaLegal,
    fesJugada,
    tauler,
    torn,
    actualitzarPartida,
    guanyador
) where

import Tauler
import Jugada
import Peca
import Posicio

type Torn = ColorPeca
data Partida = Partida Tauler Torn deriving (Eq)

tauler :: Partida -> Tauler
tauler (Partida tauler _) = tauler

torn :: Partida -> Torn
torn (Partida _ torn) = torn

actualitzarPartida :: Tauler -> Torn -> Partida
actualitzarPartida t bandol = (Partida t bandol)

-- Comprova que, la peça i la posicio de la jugada, concordi amb la disposicio del Tauler,
-- tambe comprova que la posicio desti, estigui dintre els moviments possibles de la peça
-- si tot això es correcte, comprova que la posicio desti estigui Buida
-- per ultim, comprova que no hi hagi algu entre la posicio origen i desti.
-- Si tot aixo es cert, retorna jugadaLegal -> True
jugadaLegal :: Jugada -> Tauler -> Bool
jugadaLegal j tauler | j /= Acabada = 
    let pecaMoviment = tipusPeca j
        movimentsPeca = if pecaMoviment == (buscarPeca (origen j) tauler) 
            then moviment pecaMoviment (origen j) else []
        destiPeca = ((desti j) `elem` movimentsPeca)
        esLliure = destiPeca && (((buscarPeca (desti j) tauler) == Buida) || (color (buscarPeca (desti j) tauler)) /= (color pecaMoviment))
        esLegal = if (tipus pecaMoviment /= Cavall) 
            && (tipus pecaMoviment /= Rei) 
            && ((tipus pecaMoviment /= Peo) && ((snd (origen j) /= 2) || (snd (desti j) /= 7)))
            then esLliure && not (alguEntre tauler (origen j) (desti j))
            else esLliure

    in esLegal
    | otherwise = False

fesJugada :: Tauler -> Jugada -> Tauler
fesJugada t jugada | jugada /= Acabada = 
    let esJugadaLegal = (jugadaLegal jugada t)
        nouTauler = 
            if(accio jugada) == Escac && not (escac (moure t ((tipusPeca jugada), (origen jugada)) (desti jugada)) (contrari (color (tipusPeca jugada))))  
            then error ("S'ha indicat un escac inexistent a la jugada " ++ (show jugada))
            else if (accio jugada) == EscacIMat && not (escacIMat (moure t ((tipusPeca jugada), (origen jugada)) (desti jugada)) (contrari (color (tipusPeca jugada))))  
                then error ("S'ha indicat un escac i mat inexistent a la jugada " ++ (show jugada))
                else if esJugadaLegal 
                    then (moure t ((tipusPeca jugada), (origen jugada)) (desti jugada)) 
                else error ("Jugada erronea")
    
    in nouTauler
    | otherwise = t

-- Implementar metode guanyador, que donat una Partida et retorna, no se com, el torn del guanyador
guanyador :: Tauler -> Torn
guanyador t 
            | (escacIMat t Blanc) = Negre
            | (escacIMat t Negre) = Blanc
            | otherwise = NoColor

--Instanciació de la classe de tipus "Show" per al tipus "Partida"
instance Show Partida where
    show (Partida (Tauler t) Blanc)  = show (Tauler t) 
    show (Partida (Tauler t) Negre)  = show (Tauler t)