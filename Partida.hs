module Partida (
    Partida(..),
    Tauler(..),
    crearTauler,
    torn,
    tauler,
    guanyador
) where



-- Importació de llibreries adicionals.
import Tauler
import Jugada
import Peca
import Posicio



-- Definició del tipus propis.
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

-- Funció que donat un Partida ens retorna el color del guanyador si n'hi ha, si hi ha guanyador retorna 'NoColor'.
-- * Paràmetre 1: Partida a comprovar si hi ha guanyador.
-- ** Retorn: Color del guanyador. Si no hi ha guanyador retorna 'NoColor'.
guanyador :: Partida -> String
guanyador p
    | (escacIMat (tauler p) Blanc) = "negres guanyen!!!"
    | (escacIMat (tauler p) Negre) = "blanques guanyen!!!"
    | otherwise = "empat!!!"

