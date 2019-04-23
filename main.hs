import System.IO
import Partida

main = do
    --putStrLn "Indica el nom del fitxer: "
    --nomFitxer <- getLine
    llegirContingutFitxer "./tests/pastor.txt" ReadMode (\handle -> do
        contents <- hGetContents handle
        let jugades = lines contents
        print (iniciJoc jugades))


llegirContingutFitxer :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
llegirContingutFitxer path mode f = do
    handle <- openFile path mode
    result <- f handle
    hClose handle
    return result

mostraLinia :: [String] -> String
mostraLinia [] = ""
mostraLinia (x:xs) = ((x ++ "\n") ++ mostraLinia xs )

crearPartida :: String -> Int -> Partida
crearPartida jugada n = (Partida crearTauler True (Jugada (n+1) jugada))

iniciJoc :: [String] -> Partida
iniciJoc [] = error "partida buida" 
iniciJoc [x] = crearPartida x (length x)
iniciJoc (x:xs) = iniciJoc(xs)