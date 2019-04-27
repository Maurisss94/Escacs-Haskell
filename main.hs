import System.IO
import Control.Monad
import Partida
import Jugada
import Peca
import Posicio

-- data Torn = Blanques | Negres deriving (Eq)

main = do
    --putStrLn "Indica el nom del fitxer: "
    --nomFitxer <- getLine
    let partida = (Partida crearTauler True)
    llegirContingutFitxer "./tests/pastor.txt" ReadMode (\handle -> do
        contents <- hGetContents handle
        let jugades = lines contents
        forM_ jugades $ \jugada -> do
            print ("Tirada " ++ init jugada)
            let llistaJugada = words jugada
                jugadaBlanc = jugadaBlanques llistaJugada
                jugadaNegre = if (length llistaJugada) > 2 then jugadaNegres llistaJugada else ""
                j1 = crearJugada jugadaBlanc
                j2 = crearJugada jugadaNegre
            print (j1)
            print (j2))
    


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