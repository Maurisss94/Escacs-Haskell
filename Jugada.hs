module Jugada(
    Accio(..),
    Jugada(..),
) where

import Peca 
import Posicio

data Accio = Escac | EscacIMat | Res deriving (Eq, Show)
data Jugada = Jugada {
    tipusPeca :: Peca,
    origen :: Posicio,
    desti :: Posicio,
    accio :: Accio
    } | Acabada deriving (Eq, Show)


    