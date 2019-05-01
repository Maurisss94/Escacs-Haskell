--{-# OPTIONS_GHC -Wall #-}

-- Importació de llibreries adicionals.
import System.IO
import Data.Char

-- Importació del mòduls propis.
import Partida
import Jugada
import Peca
import Posicio

-- Funció principal que mostra l'evolució de la partida.
main = do
    putStrLn "Indica el nom del fitxer: "
    nomFitxer <- getLine
    llegirContingutFitxer nomFitxer ReadMode (\handle -> do
        contents <- hGetContents handle
        let jugades = lines contents
            partidaInicial = (Partida crearTauler Blanc)
        print (crearTauler)
        mostrarJoc (tractarJugades jugades partidaInicial) jugades )

-- Funció per tractar les jugades definides en una llista de cadenes.
-- Param 1: Llista de cadenes corresponents a les jugades.
-- Param 2: Partida actual.
-- ** Retorn: Llista amb la generació de la partida.
tractarJugades :: [String] -> Partida -> [Partida]
tractarJugades [] _ = []
tractarJugades (x:xs) p = ferJugadaString x p : tractarJugades xs (ferJugadaString x p)
 
-- Funció que mou una peça a un altre posició.
-- Param 1: Tauler on es vol realitzar el moviment.
-- Param 2: Peça a moure.
-- Param 3: Posició nova a assolir.
-- ** Retorn: 'Tauler amb la peça moguda.
ferJugadaString :: String -> Partida -> Partida
ferJugadaString liniaJugada p =
    let llistaJugada = words liniaJugada
        jugadaB = jugadaBlanques llistaJugada
        jugadaN = if (length llistaJugada) > 2 then jugadaNegres llistaJugada else ""
        jB = llegirJugada jugadaB
        jN = if (length jugadaN) == 0 then Acabada else llegirJugada (map toLower jugadaN)
    in  (Partida (fesJugada (fesJugada (tauler p) jB) jN) (contrari (torn p)) )
    
-- Funció per mostrar l'evolució de la partida
-- Param 1: Evolució de la partida
-- Param 2: Llista de cadena amb el nom de la tirada
-- ** Retorn: IO corresponent a l'evolució del joc.
mostrarJoc :: [Partida] -> [String] -> IO ()
mostrarJoc [] [] = return ()
mostrarJoc [x] [s] = putStrLn ("Tirada " ++ s ++ "\n" ++ show x ++ "\n" ++ "Fi de partida, " ++ guanyador x)
mostrarJoc (p:ps) (s:ss) = do
       putStrLn ("Tirada " ++ s)
       putStrLn (show p)
       mostrarJoc ps ss

-- Funció per llegir el contingut del fitxer especificat.
-- Param 1: Ruta del fitxer.
-- Param 2: Mode d'entrada/sortida.
-- Param 3: Manegador d'entrada/sortida.
-- ** Retorn: IO correponent al contingut llegit del fitxer
llegirContingutFitxer :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
llegirContingutFitxer path mode f = do
    handle <- openFile path mode
    result <- f handle
    hClose handle
    return result

-- Funció que a partir d'una llista corresponent a una línia llegida del fitxer retorna la part corresponent a la jugadada del bàndol blanc.
-- Param 1: Llista de cadenes llegida des del fitxer
-- ** Retorn: La part corresponent a la jugadada del bàndol blanc.
jugadaBlanques :: [String] -> String
jugadaBlanques mov = (mov !! 1)

-- Funció que a partir d'una llista corresponent a una línia llegida del fitxer retorna la part corresponent a la jugadada del bàndol negre.
-- Param 1: Llista de cadenes llegida des del fitxer
-- ** Retorn: La part corresponent a la jugadada del bàndol negre.
jugadaNegres :: [String] -> String
jugadaNegres mov = (mov !! 2)