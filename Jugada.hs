module Jugada(
    Accio(..),
    Jugada(..),
    llegirJugada
) where

import Peca 
import Posicio

data Accio = Matar | Escac | EscacIMat | Res deriving (Eq, Show)
data Jugada = Jugada {
    tipusPeca :: Peca,
    origen :: Posicio,
    desti :: Posicio,
    accio :: Accio
    } | Acabada deriving (Eq, Show)


llegirJugada :: String -> Jugada
llegirJugada jugada 
    | potMatarString jugada = jugadaMatar
    | potEscacString jugada = jugadaEscac
    | potEscacIMatString jugada = jugadaEscaciMat
    | jugada == "" = Acabada
    | otherwise = jugadaNormal
    where
        peca = (obtenirPecaString jugada)
        posOrigen = ((obtenirPosicioString jugada 1), (obtenirPosicioString jugada 2))
        posDesti = ((obtenirPosicioString jugada 3), (obtenirPosicioString jugada 4))
        posDestiMatar = ((obtenirPosicioString jugada 4), (obtenirPosicioString jugada 5))
        jugadaMatar = (Jugada peca posOrigen posDestiMatar Matar)
        jugadaEscac = (Jugada peca posOrigen posDesti Escac)
        jugadaEscaciMat = (Jugada peca posOrigen posDesti EscacIMat)
        jugadaNormal = (Jugada peca posOrigen posDesti Res)



-- Funcions auxiliars que tracten el string de jugada --
potMatarString :: String -> Bool
potMatarString x = (x !! 3) == 'x'
        
potEscacString :: String -> Bool
potEscacString x = ( not (potEscacIMatString x) && (x !! (length x -1) == '+')) || ( not (potEscacIMatString x) && (potMatarString x) && (x !! (length x -1) == '+'))

potEscacIMatString :: String -> Bool
potEscacIMatString x = ((x !! (length x -1) == '+') && (x !! (length x -2) == '+')) || ((potMatarString x) && (x !! (length x -1) == '+') && (x !! (length x -2) == '+'))
        
obtenirPecaString :: String -> Peca
obtenirPecaString jugada = llegirPeca (jugada !! 0)

obtenirPosicioString :: String -> Int -> Int
obtenirPosicioString jugada x  = caracterAEnter (jugada !! x)