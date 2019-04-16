module Peca(
    TipusPeca(..)
    , ColorPeca(..)
    , Peca(..)
    , llegirPeca
) where
--Import de llibreries adicionals
import qualified Data.Char as C

--Definició de tipus
data TipusPeca = Peo | Cavall | Alfil | Torre | Dama | Rei deriving (Read)
data ColorPeca = Blanc | Negre | NoColor deriving (Show, Read)
data Peca = Peca { tipus::TipusPeca, color::ColorPeca }

--Instanciació de la classe de tipus "Show" per al tipus "TipusPeca"
instance Show TipusPeca where
    show Peo   = "p"
    show Cavall = "c"
    show Alfil = "a"
    show Torre   = "t"
    show Dama  = "d"
    show Rei   = "r"

--Instanciació de la classe de tipus "Show"  per al tipus "Peca"
instance Show Peca where
    show Peca {tipus=Peo, color=Blanc} = "P"
    show Peca {tipus=Peo, color=Negre} = "p"
    show Peca {tipus=Cavall, color=Blanc} = "C"
    show Peca {tipus=Cavall, color=Negre} = "c"
    show Peca {tipus=Alfil, color=Blanc} = "A"
    show Peca {tipus=Alfil, color=Negre} = "a"
    show Peca {tipus=Torre, color=Blanc} = "T"
    show Peca {tipus=Torre, color=Negre} = "t"
    show Peca {tipus=Dama, color=Blanc} = "D"
    show Peca {tipus=Dama, color=Negre} = "d"
    show Peca {tipus=Rei, color=Blanc} = "R"
    show Peca {tipus=Rei, color=Negre} = "r"

--Mètode per llegir una peça
llegirPeca :: Char -> Peca
llegirPeca 'P' = (Peca Peo Blanc)
llegirPeca 'p' = (Peca Peo Negre)
llegirPeca 'C' = (Peca Cavall Blanc)
llegirPeca 'c' = (Peca Cavall Negre)
llegirPeca 'A' = (Peca Alfil Blanc)
llegirPeca 'a' = (Peca Alfil Negre)
llegirPeca 'T' = (Peca Torre Blanc)
llegirPeca 't' = (Peca Torre Negre)
llegirPeca 'D' = (Peca Dama Blanc)
llegirPeca 'd' = (Peca Dama Negre)
llegirPeca 'R' = (Peca Rei Blanc)
llegirPeca 'r' = (Peca Rei Negre)
llegirPeca _ = error "No existeix la peça llegida..."

