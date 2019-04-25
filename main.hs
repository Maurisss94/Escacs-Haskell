import System.IO
import Control.Monad
import Partida
import Jugada

-- data Torn = Blanques | Negres deriving (Eq)

main = do
    --putStrLn "Indica el nom del fitxer: "
    --nomFitxer <- getLine
    let partida = (Partida crearTauler True)
    llegirContingutFitxer "./tests/pastor.txt" ReadMode (\handle -> do
        contents <- hGetContents handle
        let jugades = lines contents
        forM_ jugades $ \jugada -> do
            print (Jugada jugada))
    


llegirContingutFitxer :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
llegirContingutFitxer path mode f = do
    handle <- openFile path mode
    result <- f handle
    hClose handle
    return result

mostraLinia :: [String] -> String
mostraLinia [] = ""
mostraLinia (x:xs) = ((x ++ "\n") ++ mostraLinia xs )