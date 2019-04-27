module Jugada(
    Accio(..),
    Jugada(..),
) where

import Peca 
import Posicio

data Accio = Escac | EscacIMat | Res deriving (Eq, Show)
data Jugada = Jugada Peca Posicio Posicio Accio | Acabada deriving (Eq, Show)


    