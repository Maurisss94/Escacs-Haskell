-- Mòdul utilitzat per a representar una jugada en el joc d'escacs.
module Jugada(
    Accio(..),
    Jugada(..),
) where



-- Importació del mòdul 'Peca'.
import Peca

-- Importació del mòdul 'Posició'.
import Posicio



-- Definició del data 'Acció' necessari per representar l'acció que s'esta realitzant en cadascuna de les jugades.
data Accio = Escac | EscacIMat | Res deriving (Eq, Show)

-- Definició del data 'Jugada' necessari per representar una jugada al joc d'escacs. Jugada representada representada a partir d'un color, un origen, destí i una acció (matar, escac, escac i mat)
data Jugada = Jugada {
    tipusPeca :: Peca,
    origen :: Posicio,
    desti :: Posicio,
    accio :: Accio
    } | Acabada deriving (Eq, Show)