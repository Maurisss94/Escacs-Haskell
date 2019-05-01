module Tauler(
    Tauler(..),
    LlistaParell,
    crearTauler,
    alguEntre,
    obtenirPecesPerColor,
    buscarPeca,
    moviment,
    obtenirIndexPeca,
    moure,
    escacIMat,
    escac,
    potMatar
) where 



-- Importació del mòduls propis.
import Peca
import Posicio
import Data.List



-- Definició del tipus propis.
-- Aques tipus ens serveix per representar parells amb una peça i una posició.
type Parell = (Peca, Posicio)
-- Aques tipus ens serveix per representar llistes de parells.
type LlistaParell = [Parell]



-- Definició de tipus necessaris per representar un tauler d'escacs a partir d'una llista de parells.
data Tauler = Tauler LlistaParell deriving (Eq)



-- Instanciació de la classe de tipus 'Show' per al tipus 'Tauler'
instance Show Tauler where
    show (Tauler t) = "   " ++ replicate 10 '=' ++ "\n" ++
                      "8- |" ++ imprimirFila 8 t (reverse [1 .. 8]) ++ "|" ++ "\n" ++
                      "7- |" ++ imprimirFila 7 t (reverse [1 .. 8]) ++ "|" ++ "\n" ++
                      "6- |" ++ imprimirFila 6 t (reverse [1 .. 8]) ++ "|" ++ "\n" ++
                      "5- |" ++ imprimirFila 5 t (reverse [1 .. 8]) ++ "|" ++ "\n" ++
                      "4- |" ++ imprimirFila 4 t (reverse [1 .. 8]) ++ "|" ++ "\n" ++
                      "3- |" ++ imprimirFila 3 t (reverse [1 .. 8]) ++ "|" ++ "\n" ++
                      "2- |" ++ imprimirFila 2 t (reverse [1 .. 8]) ++ "|" ++ "\n" ++
                      "1- |" ++ imprimirFila 1 t (reverse [1 .. 8]) ++ "|" ++ "\n" ++
                      "   " ++ replicate 10 '=' ++ "\n" ++ "    abcdefgh \n" 



-- Funció auxiliar per imprimir una fila del tauler d'escacs.
-- * Paràmetre 1: Nº de fila.
-- * Paràmetre 2: Llista de parells per obtenir els valors a imprimir.
-- * Paràmetre 3: Llista d'enters necessaria per poder imprimir la fila.
-- ** Retorn: Cadena corresponent a la fila a imprimir.
imprimirFila :: Int -> LlistaParell -> [Int] -> String
imprimirFila n xs (i:is) | length is == 0 = show (iBuscarPeca (i, n) xs)
imprimirFila n xs (i:is) = imprimirFila n xs is ++ show (iBuscarPeca (i, n) xs)                      

-- Funció que genera tota la combinatoria de Posicio de dues llistes.
-- Param1: Llista d'enters.
-- Param2: Llista d'enters.
-- ** Retorn: Llistat amb totes les posicions disponibles del tauler.
generarPosicions :: [Int] -> [Int] -> [Posicio]
generarPosicions a b = [ (x, y) | y<-a, x<-b ]

-- Funció que ens retorna el tauler inicial amb les peces col·locades a la posició on toca.
-- ** Retorn: Cadena que representa un tauler en el seu estat inicial.
taulerInicial :: String
taulerInicial = intercalate "\n" ["tcadract", 
                                  "pppppppp", 
                                  "........",
                                  "........",
                                  "........",
                                  "........",
                                  "PPPPPPPP",
                                  "TCADRACT"] ++ "\n"


-- Funció per crear un tauler.
-- ** Retorn: Tauler creat.
crearTauler :: Tauler
crearTauler = Tauler (inicialitzarValorsTauler taulerInicial (generarPosicions (reverse [1 .. 8])  [1 .. 8]))

-- Funció per inicialitzar els valors del tauler amb peces i posicions
-- Param 1: String de la disposicio del tauler ex: "ptpppctp\n........\nppp"
-- Param 2: Llista de Posicions necessaries per crear la llista de Parells [(Peca, Posicio)]
-- ** Retorn: LlistaParell amb tots els valors inicials del tauler a cadascuna de les posicions.
inicialitzarValorsTauler :: String -> [Posicio] -> LlistaParell
inicialitzarValorsTauler "" []  = []
inicialitzarValorsTauler (x:xs) y | x == '\n' = inicialitzarValorsTauler xs y
inicialitzarValorsTauler (x:xs) (y:ys) | x == '.' =  [] ++ inicialitzarValorsTauler xs ys
inicialitzarValorsTauler (x:xs) (y:ys) =  [((llegirPeca x), y)] ++ inicialitzarValorsTauler xs ys

-- Funció que donada una posició retorna una peça del tauler.
-- Param 1: Posició
-- Param 2: Tauler
-- ** Retorn: Peça trobada. Si no hi ha peça retorna una peça buida.
buscarPeca :: Posicio -> Tauler -> Peca
buscarPeca pos (Tauler t) = iBuscarPeca pos t

-- Funció auxiliar per buscar una peça al tauler a partir d'una posició.
-- Param 1: Posició
-- Param 2: LlistaParell corresponent al tauler a on es vol cercar la peça.
-- ** Retorn: Peça trobada. Si no hi ha peça retorna una peça buida.
iBuscarPeca :: Posicio -> LlistaParell -> Peca
iBuscarPeca pos [] = Buida
iBuscarPeca pos llista |  not (correcte pos) = error "La posició especificada no es troba dins del taulell"
iBuscarPeca pos (x:xs) | snd x == pos = fst x
iBuscarPeca pos (x:xs) = iBuscarPeca pos xs

-- Funció que ens retorna els moviments que pot fer una peça en un tauler buit.
-- Param 1: Peça
-- Param 2: Posició
-- ** Retorn: Llistat de posicions a la que la peça es pot desplaçar en un tauler buit.
moviment :: Peca -> Posicio -> [Posicio]
moviment _ pos | not (correcte pos) = error "La posició especificada no es troba dins del taulell"
moviment peca pos
    | tipus peca == Torre = movimentsTorre pos
    | tipus peca == Cavall = movimentsCavall pos
    | tipus peca == Alfil = movimentsAlfil pos
    | tipus peca == Dama = movimentsDama pos
    | tipus peca == Rei = movimentsRei pos
    | tipus peca == Peo = movimentsPeo pos (color peca)
    | otherwise = error ("Moviments no definits")

-- Funció utilitzada per esbrinar si y es troba entre x i z (sense incloure x i z).
-- Param 1: x
-- Param 2: y
-- Param 3: z
-- ** Retorn: 'True' si la posició y es troba entre x i z, altrament 'False'.
valorEntre x y z
  |x < y = y < z
  |x > y = y > z
  |otherwise = False

-- Funció utilitzada per esbrinar si hi alguna peça entre les dues posicions que es reben per paràmetre.
-- Param 1: Tauler sobre el que es vol realitzar la comprovació.
-- Param 2: Posició inicial
-- Param 3: Posició final
-- ** Retorn: 'True' si hi ha alguna peça entre la posició inicial i la posició final, altrament 'False'
alguEntre :: Tauler -> Posicio -> Posicio -> Bool
-- Si les peces es troben a la mateixa vertical... (Fem un filtratge sobre el tauler i ens quedem amb les posicions que estan en la mateixa vertical de les posicions que ens han passat i que es troben entre els valors y de les posicions que ens ha pasat (sense incloure les p1 i p2!))
alguEntre (Tauler t) p1 p2 | (fst p1 == fst p2) = (filter (\x -> ((fst (snd x) == fst p1) && ( valorEntre (snd p1) (snd (snd x)) (snd p2))))  t) /= []
-- Si les peces es troben a la mateixa horitzontal... (Fem un filtratge sobre el tauler i ens quedem amb les posicions que estan en la mateixa horitzontal de les posicions que ens han passat i que es troben entre els valors y de les posicions que ens ha pasat (sense incloure les p1 i p2!))
alguEntre (Tauler t) p1 p2 | (snd p1 == snd p2) = (filter (\x -> ((snd (snd x) == snd p1) && ( valorEntre (fst p1) (fst (snd x)) (fst p2))))  t) /= []
-- Si les peces es troben a la mateixa diagonal... (Fem un filtratge sobre el tauler i ens quedem amb les posicions que estan en la mateixa diagonal de les posicions que ens han passat i que es troben entre els valors y de les posicions que ens ha pasat (sense incloure les p1 i p2!))
alguEntre (Tauler t) p1 p2
    -- Diagonals (++ i --)
    | ((signum(fst p1 - fst p2) == signum(snd p1 - snd p2)) && (abs(fst p1 - fst p2) == abs(snd p1 - snd p2))) = 
    (filter (\x ->  (((signum(fst p1 - fst (snd x)) == signum(snd p1 - snd (snd x))) && (abs(fst p1 - fst (snd x)) == abs(snd p1 - snd (snd x)))) &&
    ((valorEntre (fst p1) (fst (snd x)) (fst p2)) && (valorEntre (snd p1) (snd (snd x)) (snd p2))))) t) /= []
    -- Diagonals (+- i -+)
    | ((signum(fst p1 - fst p2) /= signum(snd p1 - snd p2)) && (abs(fst p1 - fst p2) == abs(snd p1 - snd p2))) = 
        (filter (\x ->  (((signum(fst p1 - fst (snd x)) /= signum(snd p1 - snd (snd x))) && (abs(fst p1 - fst (snd x)) == abs(snd p1 - snd (snd x)))) &&
        ((valorEntre (fst p1) (fst (snd x)) (fst p2)) && (valorEntre (snd p1) (snd (snd x)) (snd p2))))) t) /= []
alguEntre _ _ _ = error "No es pot calcular si hi ha alguna peça entre dues posicions si les posicions no estan en la mateixa vertical, en la mateixa horitzontal o en una de les mateixes diagonals."

-- Funció utilitzada per obtenir totes les peçes de un tauler de un determinat color.
-- Param 1: Tauler sobre el que es vol realitzar l'acció.
-- Param 2: Color de les peces que es volen obtenir.
-- ** Retorn: Llistat de les peçes del tauler amb el color especificat per paràmetre.
obtenirPecesPerColor :: Tauler -> ColorPeca -> LlistaParell
obtenirPecesPerColor (Tauler t) c = (filter (\x -> (color (fst x)) == c)  t)

-- Funció que determina si en un tauler hi ha un escac i mat.
-- Param 1: Tauler sobre el que es vol realitzar la comprovació.
-- Param 2: Color sobre el que es vol realitzar la comprovació.
-- ** Retorn: 'True' si hi ha escac i mat, altrament 'False'
escacIMat :: Tauler -> ColorPeca -> Bool
escacIMat t c = 
    let posRei = (filter (\x -> (tipus (fst x)) == Rei) (obtenirPecesPerColor t c)) !! 0 -- Cerquem el rei del color donat al tauler
        movimentsRei = filter (\x -> (buscarPeca x t) == Buida) (moviment (fst posRei) (snd posRei))  -- Dels moviments possibles ens quedem els que van a posicion lliures
        mat = if (length movimentsRei) > 0 then and (map (\x -> escac (moure t ((Peca Rei c), snd posRei) x) c) movimentsRei) else False
    in mat    

-- Funció que determina si en un tauler hi ha un escac.
-- Param 1: Tauler sobre el que es vol realitzar la comprovació.
-- Param 2: Color sobre el que es vol realitzar la comprovació.
-- ** Retorn: 'True' si hi ha escac i mat, altrament 'False'
escac :: Tauler -> ColorPeca -> Bool
escac t c = 
    let posRei = (filter (\x -> (tipus (fst x)) == Rei) (obtenirPecesPerColor t c)) !! 0 -- Cerquem el rei del color donat al tauler
        pecesContrari = obtenirPecesPerColor t (contrari c) -- Obtenim les peces del color contrari
        escac = or (map (\x -> potMatar x posRei t) pecesContrari)
    in escac

-- Funció que determina si hi ha la possibilitat de que una peça pugui matar a una altre.
-- Param 1: Peça atacant.
-- Param 2: Peça víctima.
-- Param 3: Tauler sobre el que es vol realitzar la comprovació.
-- ** Retorn: 'True' si la peça atacant pot matar a la víctima, altrament 'False' 
potMatar :: Parell -> Parell -> Tauler -> Bool
potMatar x y t =
    -- Obtenim els moviments possibles de la peça atacant (Si es un peo, ens interesen nomes els moviments d'atac!)
    let moviments = moviment (fst x) (snd x)
        pecaRival = (fst y)
        hiHaPeca = pecaRival /= Buida
        esRival = if hiHaPeca 
            then (color (fst x) /= (color pecaRival))
            else False
        mata = case tipus $ fst x of
            -- Si ataca un Peo cal comprovar si la peça a matar es troba en una de les diagonals del peo i si hi ha 'alguEntre' (si esta a primera fila)
            Peo -> (snd y) `elem` (filter (\m -> (fst (snd x) /= fst m) && (snd (snd x) /= snd m) ) moviments) && esRival
            Rei -> (snd y) `elem` moviments && esRival
            Cavall -> (snd y) `elem` moviments && esRival
            _ -> (snd y) `elem` moviments && not (alguEntre t (snd x) (snd y)) && esRival -- Torre, Alfil o Dama
    in mata

-- Funció que mou una peça a un altre posició.
-- Param 1: Tauler on es vol realitzar el moviment.
-- Param 2: Peça a moure.
-- Param 3: Posició nova a assolir.
-- ** Retorn: 'Tauler amb la peça moguda.
moure :: Tauler -> Parell -> Posicio -> Tauler
moure (Tauler t) parell pos = Tauler ((fst parell, pos) : (delete parell t))

-- Funció auxiliar per obtenir l'index d'un parell a la llista de parells dels tauler.
-- Param 1: Parell a cercar.
-- Param 2: LlistaParell on cercar el parell.
-- ** Retorn: Valor corresponent a l'index del parell a la LlistaParell, altrament retorna -1.
obtenirIndexPeca :: Parell -> LlistaParell -> Int
obtenirIndexPeca i xs =
    case i `elemIndex` xs of
       Just n  -> n
       Nothing -> -1