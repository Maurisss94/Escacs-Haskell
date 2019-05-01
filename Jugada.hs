module Jugada(
    Accio(..),
    Jugada(..),
    llegirJugada,
    fesJugada,
    obligatMatar
) where



-- Importació del mòduls propis.
import Peca
import Posicio
import Tauler



-- Instanciació de la classe de tipus 'Show' per al tipus 'TipusPeca'.
instance Show Jugada where
    show (Jugada p origen desti accio) = "amb peça (" ++ (show p) ++ ") " ++ ", posicio origen " ++ (show origen) ++ ", posicio destí " ++ (show desti) ++ " i accio " ++ (show accio) ++ "."
    show Acabada = " " ++  "."

instance Show Accio where
    show Matar = "captura"  
    show Escac = "escac"
    show EscacIMat = "escac i mat"
    show Res = "moure"

data Accio = Matar | Escac | EscacIMat | Res deriving (Eq)
data Jugada = Jugada {
    tipusPeca :: Peca,
    origen :: Posicio,
    desti :: Posicio,
    accio :: Accio
    } | Acabada deriving (Eq)


    
-- Funció per llegir una jugada. A partir d'una cadena ens retorna la jugada corresponent.
-- * Paràmetre 1: String d'on extreure la jugada
-- ** Retorn: Una jugada de tipus 'Jugada'.
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


-- Funció que certifica que la Jugada proposada és legal a partir. 
-- * Paràmetre 1: Jugada 
-- * Paràmetre 2: Tauler 
-- ** Retorn: Llista de posicions (moviments) possibles per a la peça tenint en compte la posició on es troba.
jugadaLegal :: Jugada -> Tauler -> Bool
jugadaLegal j tauler | j /= Acabada = 
    let pecaMoviment = tipusPeca j
        moviments = if pecaMoviment == (buscarPeca (origen j) tauler) 
            then moviment pecaMoviment (origen j) else []
        movimentValid = ((desti j) `elem` moviments) -- El moviment esta en la llista de possibles moviments
        posicioBuida = ((buscarPeca (desti j) tauler) == Buida) -- La posició destí es buida
        destiColorContrari = (not posicioBuida) && ((color (buscarPeca (desti j) tauler)) /= (color pecaMoviment))  -- A la posició destí hi ha una peça de l'adversari
        desplDiagonal = ((desti j) `elem` (filter (\m -> (fst (origen j) /= fst m) && (snd (origen j) /= snd m)) moviments)) -- Desplaçament diagonal
        peoPotMatarDiagonal = desplDiagonal -- Variable per comprovar si un peo pot matar en diagonal
        algunaPecaEntre = (alguEntre tauler (origen j) (desti j)) -- Hi ha algun entre (desplaçaments llargs)
        esLegal =
            if (tipus pecaMoviment == Cavall || tipus pecaMoviment == Rei) 
            then (movimentValid && (posicioBuida || destiColorContrari))
            else if (tipus pecaMoviment == Peo && movimentValid) then
                if (peoPotMatarDiagonal) then ((buscarPeca (desti j) tauler) /= Buida)
                else if ((snd (origen j) /= 2) || (snd (desti j) /= 7)) then posicioBuida
                else if ((snd (origen j) == 2) || (snd (desti j) == 7)) then ((not algunaPecaEntre) && posicioBuida)
                else False
            else
                movimentValid && ((not algunaPecaEntre) || destiColorContrari) -- Dama, alfil i torre
    in esLegal
    | otherwise = False
 

-- Funció que donat un Tauler i una Jugada ens torna un nou Tauler amb la jugada feta.
-- * Paràmetre 1: Tauler inicial
-- * Paràmetre 2: Tauler amb la jugada aplicada
-- ** Retorn: Tauler amb la jugada feta
fesJugada :: Tauler -> Jugada -> Tauler
fesJugada t jugada | jugada /= Acabada = 
    let esJugadaLegal = (jugadaLegal jugada t)
        taulerAmbJugada = (moure t ((tipusPeca jugada), (origen jugada)) (desti jugada))
        nouTauler = 
            if obligatMatar t jugada && (accio jugada) == Res
            then error ("No s'ha indicat la captura a la jugada " ++ (show jugada))
            else if (accio jugada) == Matar && ((buscarPeca (desti jugada) t) == Buida)
            then error ("S'ha especificat una captura inexistent a la jugada " ++ (show jugada))
            else if (accio jugada) == Escac && not (escac taulerAmbJugada (contrari (color (tipusPeca jugada))))  
            then error ("S'ha indicat un escac inexistent a la jugada " ++ (show jugada))
            else if (accio jugada) == EscacIMat && not (escacIMat taulerAmbJugada (contrari (color (tipusPeca jugada))))  
                then error ("S'ha indicat un escac i mat inexistent a la jugada " ++ (show jugada))
                else if esJugadaLegal 
                    then taulerAmbJugada 
                else error ("La jugada " ++ (show jugada) ++ " Es errònia.")
    
    in nouTauler
    | otherwise = t

-- Funció que determina si en una jugada caldria matar o no
-- * Paràmetre 1: Tauler actual.
-- * Paràmetre 2: Jugada actual.
-- ** Retorn: 'True' si en la jugada cal matar, altrament retorna 'False'.
obligatMatar :: Tauler -> Jugada -> Bool
obligatMatar t jug = 
    let pecaOri = (tipusPeca jug)
        des = (desti jug)
        pecaDes = buscarPeca des t
        ori = (origen jug)
     in or (map (\x -> (potMatar (pecaOri, ori) (buscarPeca x t, x) t) && (buscarPeca x t /= Buida)) (moviment pecaOri ori))

-- Funció que ens serveix per determinar si en una cadena llegida des del fitxer s'especifica una captura o no.
-- * Paràmetre 1: String a comprovar (p.e 'Pe5xf4') 
-- ** Retorn: 'True' si la cadena conté una 'x' a la posició 3, altrament retorna 'False'.
potMatarString :: String -> Bool
potMatarString x = (x !! 3) == 'x'
       
-- Funció que ens serveix per determinar si en una cadena llegida des del fitxer s'especifica una escac o no.
-- * Paràmetre 1: String a comprovar (p.e 'Dh4xg3+') 
-- ** Retorn: 'True' si la cadena conté una '+' al final de la cadena, altrament retorna 'False'.
potEscacString :: String -> Bool
potEscacString x = ( not (potEscacIMatString x) && (x !! (length x -1) == '+')) || ( not (potEscacIMatString x) && (potMatarString x) && (x !! (length x -1) == '+'))

-- Funció que ens serveix per determinar si en una cadena llegida des del fitxer s'especifica una escac o no.
-- * Paràmetre 1: String a comprovar (p.e 'Dh4xg3+') 
-- ** Retorn: 'True' si la cadena conté una '+' al final de la cadena, altrament retorna 'False'.
potEscacIMatString :: String -> Bool
potEscacIMatString x = ((x !! (length x -1) == '+') && (x !! (length x -2) == '+')) || ((potMatarString x) && (x !! (length x -1) == '+') && (x !! (length x -2) == '+'))

-- Funció que ens serveix per extreure una peça de una cadena llegida des del fitxer
-- * Paràmetre 1: String a comprovar (p.e 'Dh4g3') 
-- ** Retorn: Una peça de tipus 'Peca' corresponent a la peça llegida.
obtenirPecaString :: String -> Peca
obtenirPecaString jugada = llegirPeca (jugada !! 0)

-- Funció que ens serveix per extreure una peça de una cadena llegida des del fitxer
-- * Paràmetre 1: String a comprovar (p.e 'Dh4g3') 
-- ** Retorn: Una peça de tipus 'Peca' corresponent a la peça llegida.
obtenirPosicioString :: String -> Int -> Int
obtenirPosicioString jugada x  = caracterAEnter (jugada !! x)
