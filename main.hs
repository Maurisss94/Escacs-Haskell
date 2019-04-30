--{-# OPTIONS_GHC -Wall #-}

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
        mostrarJoc (tractarJugades jugades partidaInicial) jugades )


tractarJugades :: [String] -> Partida -> [Partida]
tractarJugades [] _ = []
tractarJugades (x:xs) p = ferJugadaString x p : tractarJugades xs (ferJugadaString x p)
 
ferJugadaString :: String -> Partida -> Partida
ferJugadaString liniaJugada p =
    let llistaJugada = words liniaJugada
        jugadaB = jugadaBlanques llistaJugada
        jugadaN = if (length llistaJugada) > 2 then jugadaNegres llistaJugada else ""
        jB = llegirJugada jugadaB
        jN = if (length jugadaN) == 0 then Acabada else llegirJugada (map toLower jugadaN)
    in  (Partida (fesJugada (fesJugada (tauler p) jB) jN) (contrari (torn p)) )
    
-- Rep una Llista de Partides, i les linies del fitxer a ser impreses.
mostrarJoc :: [Partida] -> [String] -> IO ()
mostrarJoc [] [] = return ()
mostrarJoc [x] [s] = putStrLn ("Tirada " ++ s ++ "\n" ++ show x ++ show (guanyador x))
mostrarJoc (p:ps) (s:ss) = do
       putStrLn ("Tirada " ++ s)
       putStrLn (show p)
       mostrarJoc ps ss

llegirContingutFitxer :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
llegirContingutFitxer path mode f = do
    handle <- openFile path mode
    result <- f handle
    hClose handle
    return result


jugadaBlanques :: [String] -> String
jugadaBlanques mov = (mov !! 1)

jugadaNegres :: [String] -> String
jugadaNegres mov = (mov !! 2)