module Jugada(
    Jugada(..)
) where

data Jugada = Jugada Int String deriving (Eq)

instance Show Jugada where
    show (Jugada n j) = "Tirada " ++ show n ++ ". " ++ j
    