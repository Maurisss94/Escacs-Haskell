{-# OPTIONS_GHC -Wall #-}

import System.IO
import Data.Char
import Partida
import Jugada
import Peca
import Posicio


main = do
    --putStrLn "Indica el nom del fitxer: "
    --nomFitxer <- getLine
    llegirContingutFitxer "./tests/pastor.txt" ReadMode (\handle -> do
        contents <- hGetContents handle
        let jugades = lines contents
            partidaInicial = (Partida crearTauler Blanc)
        print (crearTauler)
        imprimirJoc (tractarJugades jugades partidaInicial) jugades )


tractarJugades :: [String] -> Partida -> [Partida]
tractarJugades [] _ = []
tractarJugades (x:xs) p = realitzarAccio x p : tractarJugades xs (realitzarAccio x p)
 
realitzarAccio :: String -> Partida -> Partida
realitzarAccio l p =
    let accio = words l
        jugadaB = jugadaBlanques accio
        jugadaN = if (length accio) > 2 then jugadaNegres accio else ""
        jB = crearJugada jugadaB
        jN = if (length jugadaN) == 0 then Acabada else crearJugada (map toLower jugadaN)
    in  (Partida (fesJugada (fesJugada (tauler p) jB) jN) (contrari (torn p)) )
    
-- Rep una Llista de Partides, i les linies del fitxer a ser impreses.
imprimirJoc :: [Partida] -> [String] -> IO ()
imprimirJoc [] [] = return ()
imprimirJoc [x] [s] = putStrLn ("Tirada " ++ s ++ "\n" ++ show x ++ show (guanyador (tauler x)))
imprimirJoc (p:ps) (s:ss) = do
       putStrLn ("Tirada " ++ s)
       putStrLn (show p)
       imprimirJoc ps ss

crearJugada :: String -> Jugada
crearJugada jugada =
    let peca = (obtenirPecaPartida jugada)
        posOrigen = ((obtenirPosicioPartida jugada 1), (obtenirPosicioPartida jugada 2))
        matar = potMatar jugada
        posDesti = if matar then ((obtenirPosicioPartida jugada 4), (obtenirPosicioPartida jugada 5))
            else ((obtenirPosicioPartida jugada 3), (obtenirPosicioPartida jugada 4))
        accio = if potEscac jugada 
            then Escac 
            else if potEscacIMat jugada
                then EscacIMat
                else Res
        res = if jugada /= "" then (Jugada peca posOrigen posDesti accio) else Acabada
    in res

potMatar :: String -> Bool
potMatar x = (x !! 3) == 'x'

potEscac :: String -> Bool
potEscac x = ( not (potEscacIMat x) && (x !! (length x -1) == '+')) || ( not (potEscacIMat x) && (potMatar x) && (x !! (length x -1) == '+'))

potEscacIMat :: String -> Bool
potEscacIMat x = ((x !! (length x -1) == '+') && (x !! (length x -2) == '+')) || ((potMatar x) && (x !! (length x -1) == '+') && (x !! (length x -2) == '+'))

llegirContingutFitxer :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
llegirContingutFitxer path mode f = do
    handle <- openFile path mode
    result <- f handle
    hClose handle
    return result

obtenirPecaPartida :: String -> Peca
obtenirPecaPartida jugada = llegirPeca (jugada !! 0)

obtenirPosicioPartida :: String -> Int -> Int
obtenirPosicioPartida jugada x  = caracterAEnter (jugada !! x)

jugadaBlanques :: [String] -> String
jugadaBlanques mov = (mov !! 1)

jugadaNegres :: [String] -> String
jugadaNegres mov = (mov !! 2)