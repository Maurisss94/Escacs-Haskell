module Jugada(
    Jugada(..)
) where

data Jugada = Jugada String deriving (Eq)

numJugada :: Jugada -> String
numJugada jugada = (head (jugades jugada))

jugades :: Jugada -> [String]
jugades (Jugada moviment) = tail (split ' ' moviment)

split :: Char -> String -> [String]
split _ [] = [""]
split del (c:cs) | c == del  = "" : rest
             | otherwise = (c : head rest) : tail rest
    where rest = split del cs

instance Show Jugada where
    show (Jugada j) = "Tirada " ++ j


    