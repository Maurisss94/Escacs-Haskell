module Peca(
    Peca(..),
    TipusPeca(..),
    ColorPeca(..),
    llegirPeca,
    movimentsPeo,
    movimentsAlfil,
    movimentsTorre,
    movimentsDama,
    movimentsRei
) where


--Import de llibreries adicionals
import qualified Data.Char as C

import Posicio

--Definició de tipus
data TipusPeca = Peo | Cavall | Alfil | Torre | Dama | Rei deriving (Read)
data ColorPeca = Blanc | Negre | NoColor deriving (Eq, Show, Read)
data Peca = Peca { tipus::TipusPeca, color::ColorPeca } | Buida deriving (Eq)

--Instanciació de la classe de tipus "Show" per al tipus "TipusPeca"
instance Show TipusPeca where
    show Peo   = "p"
    show Cavall = "c"
    show Alfil = "a"
    show Torre   = "t"
    show Dama  = "d"
    show Rei   = "r"

--Instanciació de la classe de tipus "Eq" per al tipus "TipusPeca"
instance Eq TipusPeca where
    (==) Peo Peo = True
    (==) Cavall Cavall = True
    (==) Alfil Alfil = True
    (==) Torre Torre = True
    (==) Dama Dama = True
    (==) Rei Rei = True
    (==) _ _ = False
    
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
    show Buida = "."   

--Caldria instanciar "Read" per al tipus "Peca", per no haver d'utilitzar tota l'estona "llegirPeca"..
--instance Read Peca where
--    readsPrec _ = llegirPeca

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


movimentsPeo :: Posicio -> [Posicio]
movimentsPeo pos | (fst pos == 1) && (snd pos == 2) = [(x,y)|x <- [fst pos], y <- [snd pos + 1, snd pos + 2]] ++ [(fst pos + 1, snd pos + 1)]
movimentsPeo pos | (fst pos == 8) && (snd pos == 2) = [(x,y)|x <- [fst pos], y <- [snd pos + 1, snd pos + 2]] ++ [(fst pos - 1, snd pos + 1)]
movimentsPeo pos | (fst pos == 1) = [(x,y)|x <- [fst pos], y <- [snd pos + 1]] ++ [(fst pos + 1, snd pos + 1)]
movimentsPeo pos | (fst pos == 8) = [(x,y)|x <- [fst pos], y <- [snd pos + 1]] ++ [(fst pos - 1, snd pos + 1)]
movimentsPeo pos = [(x,y)|x <- [fst pos], y <- [snd pos + 1, snd pos + 2]] ++ [(fst pos - 1, snd pos + 1)] ++ [(fst pos + 1, snd pos + 1)]

-- Obtenir moviments de l'alfil
-- Primer zip: Genera les posicions desde x fins a 8, i desde y fins a 8 (Diagonals dreta superior)
-- Segon zip: Genera les posicions desde x fins a 1, i desde y fins a 1 (Diagonals esquerra inferior)
-- Tercer zip: Genera les posicions desde x fins a 8, i desde y fins a 1 (Diagonals dreta inferior)
-- Quart zip: Genera les posicions desde x fins a 1, i desde y fins a 8 (Diagonals esquerra superior)
movimentsAlfil :: Posicio -> [Posicio]
movimentsAlfil pos = zip [fst pos + 1 .. 8] [snd pos + 1 .. 8] ++ (zip (reverse [1 .. fst pos -1]) (reverse[1 .. snd pos -1])) ++ (zip [fst pos + 1 .. 8] (reverse[1 .. snd pos -1])) ++ (zip (reverse [1 .. fst pos -1]) [snd pos + 1 .. 8])

-- Obtenir moviments de la torre
-- Generació primera llista: Es generen totes les posicions desde la posicio actual reduïnt les x (desplaçament horitzontal dret (inici a pos actual))
-- Generació segona llista: Es generen totes les posicions desde la posicio actual augmentat les x (desplaçament horitzontal dret (pos actual a ultima casella))
-- Generació tercera llista: Es generen totes les posicions desde la posicio actual reduïnt les y (desplaçament vertical (inici a pos actual))
-- Generació quarta llista: Es generen totes les posicions desde la posicio actual augmentant les y (desplaçament vertical (pos actual a ultima casella))
movimentsTorre :: Posicio -> [Posicio]
movimentsTorre pos  = [(x,y)|x <- [1 .. fst pos - 1], y <- [snd pos]] ++ [(x,y)|x <- [fst pos +1 .. 8], y <- [snd pos]] ++ [(x,y)|x <- [fst pos], y <- [1 .. snd pos - 1]] ++ [(x,y)|x <- [fst pos], y <- [snd pos +1 .. 8]]


-- Obtenir moviments de la dama
-- Aprofitem els moviments de l'alfil i de la torre per generar els moviments de la dama
movimentsDama :: Posicio -> [Posicio]
movimentsDama pos = movimentsAlfil pos ++ movimentsTorre pos

-- Obtenir moviments del rei
-- Aprofitem els moviments de la dama limitant el deplaçament a una casella
movimentsRei :: Posicio -> [Posicio]
movimentsRei pos = filter (\x -> ((abs (fst pos - fst x) <= 1) && (abs (snd pos - snd x) <= 1))) (movimentsDama pos)

-- Obtenir els moviments del cavall
-- A partir dels moviments posibles del cavall, es generen totes les possibilitats desde la posició actual, filtrant les posicions que es troben dins del tauler
movimentsCavall :: Posicio -> [Posicio]
movimentsCavall pos = filter (\x -> correcte x) [(fst x + fst y, snd x + snd y)|x <- [pos], y <- [(1,2),(2,1),(-1,2),(2,-1),(-2,1),(1,-2),(-1,-2),(-2,-1)]]