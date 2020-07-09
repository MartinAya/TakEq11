module Prueba2 where

import Data.Maybe
import Data.List
import Data.Char
import System.Random --para que cargue bien usar stack ghci
import Data.List.Split
import Control.Monad


type Tablero = [Casilla]
data Casilla = ConstructorCasilla [Ficha] deriving (Eq,Show)
data Ficha = Horizontal TakPlayer | Vertical TakPlayer deriving (Eq,Show)
data TakPlayer = WhitePlayer | BlackPlayer deriving (Eq, Enum, Bounded,Show)
data TakGame = ConstructorTakGame Tablero TakPlayer deriving (Eq,Show)

beginning3x3 :: TakGame -- El estado inicial del juego Tak con un tablero de 3x3, con el tablero vacío. 
beginning3x3 = ConstructorTakGame [ ConstructorCasilla [] | _ <- [1..9]] WhitePlayer

beginning4x4 :: TakGame -- El estado inicial del juego Tak con un tablero de 4x4, con el tablero vacío. 
beginning4x4 = ConstructorTakGame [ ConstructorCasilla [] | _ <- [1..16]] WhitePlayer

getTablero :: TakGame -> Tablero
getTablero (ConstructorTakGame tablero _) = tablero

coordenadasNxN :: Int -> [(Int,Int)]
coordenadasNxN largoTablero = stringIndices
   where
      largoFila = (floor (sqrt (fromIntegral largoTablero)))
      stringIndices = map (\n -> divMod n largoFila ) [0..(largoTablero-1)]

coordenadasNxNT :: Tablero -> [(Int,Int)]
coordenadasNxNT tablero = coordenadasNxN (length tablero)


intANxN :: Tablero -> Int -> (Int,Int)
intANxN tablero posicion = (coordenadasNxN largo)!!posicion
   where
      largo = length tablero

nxNAint :: Tablero -> (Int,Int) -> Int
nxNAint tablero tupla = if (resultado==Nothing) 
   then error "coordenadas no válidas" 
   else fromMaybe 0 resultado
      --Nothing
      --Just 8
      where 
         largo = length tablero
         resultado = elemIndex tupla (coordenadasNxN largo)