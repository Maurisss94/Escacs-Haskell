-- Mòdul utilitzat per a representar una partida en el joc d'escacs. La
module Partida (
    Partida(..),
    Tauler(..),
    crearTauler,
    fesJugada,
    torn,
    tauler,
    guanyador
) where


-- Importació de llibreries adicionals.
import Tauler
import Jugada
import Peca
import Posicio


-- Definició del tipus 'Torn'.
type Torn = ColorPeca



-- Definició de tipus necessaris per representar una partida d'escacs a partir d'un tauler i un torn.
data Partida = Partida Tauler Torn deriving (Eq)



-- Instanciació de la classe de tipus 'Show' per al tipus 'Partida'
instance Show Partida where
    show (Partida (Tauler t) Blanc)  = show (Tauler t) 
    show (Partida (Tauler t) Negre)  = show (Tauler t)


    
-- Funció per obtenir un el tauler d'una partida.
-- * Paràmetre 1: Partida de la qual s'extraurà el tauler.
-- ** Retorn: Tauler de la partida rebuda per paràmetre.
tauler :: Partida -> Tauler
tauler (Partida tauler _) = tauler

-- Funció per obtenir un el torn d'una partida.
-- * Paràmetre 1: Partida de la qual s'extraurà el torn.
-- ** Retorn: Torn corresponent a la partida rebuda per paràmetre.
torn :: Partida -> Torn
torn (Partida _ torn) = torn


-- Funció que certifica que la Jugada proposada és legal a partir. 
-- * Paràmetre 1: Jugada 
-- * Paràmetre 2: Tauler 
-- ** Retorn: Llista de posicions (moviments) possibles per a la peça tenint en compte la posició on es troba.
-- *** Aclariments
-- Comprova que, la peça i la posicio de la jugada, concordi amb la disposicio del Tauler,
-- Tambe comprova que la posicio desti, estigui dintre els moviments possibles de la peça
-- Si tot això es correcte, comprova que la posicio desti estigui Buida
-- Per ultim, comprova que no hi hagi algu entre la posicio origen i desti.
-- Si tot aixo es cert, retorna jugadaLegal -> True
jugadaLegal :: Jugada -> Tauler -> Bool
jugadaLegal j tauler | j /= Acabada = 
    let pecaMoviment = tipusPeca j
        movimentsPeca = if pecaMoviment == (buscarPeca (origen j) tauler) 
            then moviment pecaMoviment (origen j) else []
        destiPeca = ((desti j) `elem` movimentsPeca)
        esLliure = destiPeca && (((buscarPeca (desti j) tauler) == Buida) || (color (buscarPeca (desti j) tauler)) /= (color pecaMoviment))
        esLegal = if (tipus pecaMoviment /= Cavall) 
            && (tipus pecaMoviment /= Rei) 
            && ((tipus pecaMoviment /= Peo) && ((snd (origen j) /= 2) || (snd (desti j) /= 7)))
            then esLliure && not (alguEntre tauler (origen j) (desti j))
            else esLliure

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
            if(accio jugada) == Escac && not (escac taulerAmbJugada (contrari (color (tipusPeca jugada))))  
            then error ("S'ha indicat un escac inexistent a la jugada " ++ (show jugada))
            else if (accio jugada) == EscacIMat && not (escacIMat taulerAmbJugada (contrari (color (tipusPeca jugada))))  
                then error ("S'ha indicat un escac i mat inexistent a la jugada " ++ (show jugada))
                else if esJugadaLegal 
                    then taulerAmbJugada 
                else error ("Jugada erronea")
    
    in nouTauler
    | otherwise = t

-- Funció que donat un Partida ens retorna el color del guanyador si n'hi ha, si hi ha guanyador retorna 'NoColor'.
-- * Paràmetre 1: Partida a comprovar si hi ha guanyador.
-- ** Retorn: Color del guanyador. Si no hi ha guanyador retorna 'NoColor'.
guanyador :: Partida -> Torn
guanyador p
    | (escacIMat (tauler p) Blanc) = Negre
    | (escacIMat (tauler p) Negre) = Blanc
    | otherwise = NoColor
