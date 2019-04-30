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

import Peca
import Posicio
import Data.List


type Parell = (Peca, Posicio)
type LlistaParell = [Parell]

data Tauler = Tauler LlistaParell deriving (Eq)


                    
imprimirFila :: Int -> LlistaParell -> [Int] -> String
imprimirFila n xs (i:is) | length is == 0 = show (iBuscarPeca (i, n) xs)
imprimirFila n xs (i:is) = imprimirFila n xs is ++ show (iBuscarPeca (i, n) xs)                      


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





obtenirLlistaTauler :: Tauler -> LlistaParell
obtenirLlistaTauler (Tauler t) = t

-- Metode que genera tota la combinatoria de Posicio de dues llistes
-- Param1: Llista d'enters
-- Param2: Llista d'enters
generarPosicions :: [Int] -> [Int] -> [Posicio]
generarPosicions a b = [ (x, y) | y<-a, x<-b ]

-- String que representa l'estat inicial del tauler d'escacs
-- Les peces representades en minuscula, son les negres, les majuscules son les blanques.
taulerInicial :: String
taulerInicial = intercalate "\n" ["tcadract", 
                                  "pppppppp", 
                                  "........",
                                  "........",
                                  "........",
                                  "........",
                                  "PPPPPPPP",
                                  "TCADRACT"] ++ "\n"

-- taulerInicial :: String
-- taulerInicial = intercalate "\n" ["t.adra.t", 
--                                   "pppp.Dpp", 
--                                   "..c..c..",
--                                   "....p...",
--                                   "..A.P...",
--                                   "........",
--                                   "PPPP.PPP",
--                                   "TCA.R.CT"] ++ "\n"

-- taulerInicial :: String
-- taulerInicial = intercalate "\n" ["t.adra.t", 
--                                   "pppp.ppp", 
--                                   "..c..c..",
--                                   "....p..D",
--                                   "..A.P...",
--                                   "........",
--                                   "PPPP.PPP",
--                                   "TCA.R.CT"] ++ "\n"

-- taulerInicial :: String
-- taulerInicial = intercalate "\n" ["t.adra.t", 
--                                   "pppp.Dpp", 
--                                   "..c..c..",
--                                   "....p...",
--                                   "..A.P...",
--                                   "........",
--                                   "PPPP.PPP",
--                                   "TCA.R.CT"] ++ "\n"


-- Param 1: String que representa l'estat inicial d'un tauler d'escacs.
-- Return: Retorna un tipus Tauler amb les posicions i peces.
crearTauler :: Tauler
crearTauler = Tauler (inicialitzarValorsTauler taulerInicial (generarPosicions (reverse [1 .. 8])  [1 .. 8]))

-- Inicialitza els valors del tauler amb peces i posicions
-- Param 1: String de la disposicio del tauler ex: "ptpppctp\n........\nppp"
-- Param 2: Llista de Posicions necessaries per crear la llista de Parells [(Peca, Posicio)]
inicialitzarValorsTauler :: String -> [Posicio] -> LlistaParell
inicialitzarValorsTauler "" []  = []
inicialitzarValorsTauler (x:xs) y | x == '\n' = inicialitzarValorsTauler xs y
inicialitzarValorsTauler (x:xs) (y:ys) | x == '.' =  [] ++ inicialitzarValorsTauler xs ys
inicialitzarValorsTauler (x:xs) (y:ys) =  [((llegirPeca x), y)] ++ inicialitzarValorsTauler xs ys

buscarPeca :: Posicio -> Tauler -> Peca
buscarPeca pos (Tauler t) = iBuscarPeca pos t

iBuscarPeca :: Posicio -> LlistaParell -> Peca
iBuscarPeca pos [] = Buida
iBuscarPeca pos llista |  not (correcte pos) = error "La posició especificada no es troba dins del taulell"
iBuscarPeca pos (x:xs) | snd x == pos = fst x
iBuscarPeca pos (x:xs) = iBuscarPeca pos xs

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


-- Mètode per esbrinar si y es troba entre x i z (sense incloure x i z)
valorEntre x y z
  |x < y = y < z
  |x > y = y > z
  |otherwise = False

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


obtenirPecesPerColor :: Tauler -> ColorPeca -> LlistaParell
obtenirPecesPerColor (Tauler t) c = (filter (\x -> (color (fst x)) == c)  t)



escacIMat :: Tauler -> ColorPeca -> Bool
escacIMat t c = 
    let posRei = (filter (\x -> (tipus (fst x)) == Rei) (obtenirPecesPerColor t c)) !! 0 -- Cerquem el rei del color donat al tauler
        movimentsRei = filter (\x -> (buscarPeca x t) == Buida) (moviment (fst posRei) (snd posRei))  -- Dels moviments possibles ens quedem els que van a posicion lliures
        mat = if (length movimentsRei) > 0 then and (map (\x -> escac t c) movimentsRei) else False
    in mat    


escac :: Tauler -> ColorPeca -> Bool
escac t c = 
    let posRei = (filter (\x -> (tipus (fst x)) == Rei) (obtenirPecesPerColor t c)) !! 0 -- Cerquem el rei del color donat al tauler
        pecesContrari = obtenirPecesPerColor t (contrari c) -- Obtenim les peces del color contrari
        escac = or (map (\x -> potMatar x posRei t) pecesContrari)
    in escac



potMatar :: Parell -> Parell -> Tauler -> Bool
potMatar x y t =
    -- Obtenim els moviments possibles de la peça atacant (Si es un peo, ens interesen nomes els moviments d'atac!)
    let moviments = moviment (fst x) (snd x)
        mata = case tipus $ fst x of
            -- Si ataca un Peo cal comprovar si la peça a matar es troba en una de les diagonals del peo i si hi ha 'alguEntre' (si esta a primera fila)
            Peo -> (snd y) `elem` (filter (\m -> (fst (snd x) /= fst m) && (snd (snd x) /= snd m) ) moviments)
            Rei -> (snd y) `elem` moviments
            Cavall -> (snd y) `elem` moviments
            _ -> (snd y) `elem` moviments && not (alguEntre t (snd x) (snd y)) -- Torre, Alfil o Dama
    in mata


moure :: Tauler -> Parell -> Posicio -> Tauler
moure (Tauler t) parell pos = Tauler ((fst parell, pos) : (delete parell t))


obtenirIndexPeca :: Parell -> LlistaParell -> Int
obtenirIndexPeca i xs =
    case i `elemIndex` xs of
       Just n  -> n
       Nothing -> -1