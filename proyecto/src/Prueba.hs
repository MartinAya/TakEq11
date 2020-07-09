module Prueba2 where

import Data.Maybe
import Data.List
import Data.Char
import System.Random --para que cargue bien usar stack ghci
import Data.List.Split
import Control.Monad
import Data.List ((\\))


type Tablero = [Casilla]
data Casilla = ConstructorCasilla [Ficha] deriving (Eq,Show)
data Ficha = Horizontal TakPlayer | Vertical TakPlayer deriving (Eq,Show)
data TakPlayer = WhitePlayer | BlackPlayer deriving (Eq, Enum, Bounded,Show)
data TakGame = ConstructorTakGame Tablero TakPlayer deriving (Eq,Show)



type Coord = (Int, Int)
type Path = [Coord]


camino3x3 :: Tablero -> TakPlayer -> Bool
camino3x3 tablero jugador
   |posicionesLlenas tableroSoloConFichasJugador [0,3,6] = True
   |posicionesLlenas tableroSoloConFichasJugador [1,4,7] = True
   |posicionesLlenas tableroSoloConFichasJugador [2,5,8] = True

   |posicionesLlenas tableroSoloConFichasJugador [0,1,2] = True
   |posicionesLlenas tableroSoloConFichasJugador [3,4,5] = True
   |posicionesLlenas tableroSoloConFichasJugador [6,7,8] = True

   |posicionesLlenas tableroSoloConFichasJugador [0,3,4,7] = True
   |posicionesLlenas tableroSoloConFichasJugador [1,4,5,8] = True
   |posicionesLlenas tableroSoloConFichasJugador [2,5,4,7] = True
   |posicionesLlenas tableroSoloConFichasJugador [1,4,3,6] = True
   |posicionesLlenas tableroSoloConFichasJugador [0,1,4,5] = True
   |posicionesLlenas tableroSoloConFichasJugador [3,4,7,8] = True
   |posicionesLlenas tableroSoloConFichasJugador [6,7,4,5] = True
   |posicionesLlenas tableroSoloConFichasJugador [3,4,1,2] = True
   |otherwise = False
      where
         tableroSoloConFichasJugador = map (vaciarCasillaSiNoEsDeJugador jugador) tablero

camino4x4 :: Tablero -> TakPlayer -> Bool
camino4x4 tablero jugador
   |posicionesLlenas tableroSoloConFichasJugador [0,1,2,3] = True
   |posicionesLlenas tableroSoloConFichasJugador [4,5,6,7] = True
   |posicionesLlenas tableroSoloConFichasJugador [8,9,10,11] = True
   |posicionesLlenas tableroSoloConFichasJugador [12,13,14,15] = True
   |posicionesLlenas tableroSoloConFichasJugador [0,4,8,12] = True
   |posicionesLlenas tableroSoloConFichasJugador [1,5,9,13] = True
   |posicionesLlenas tableroSoloConFichasJugador [2,6,10,14] = True
   |posicionesLlenas tableroSoloConFichasJugador [3,7,11,15] = True
   |posicionesLlenas tableroSoloConFichasJugador [0,4,5,9,13] = True
   |posicionesLlenas tableroSoloConFichasJugador [0,4,5,6,10,14] = True
   |posicionesLlenas tableroSoloConFichasJugador [0,4,5,9,10,14] = True
   |posicionesLlenas tableroSoloConFichasJugador [0,4,5,9,13] = True
   |posicionesLlenas tableroSoloConFichasJugador [0,4,8,9,10,14] = True
   |posicionesLlenas tableroSoloConFichasJugador [1,5,4,8,12] = True
   |posicionesLlenas tableroSoloConFichasJugador [1,5,9,8,12] = True
   |posicionesLlenas tableroSoloConFichasJugador [1,5,6,10,14] = True
   |posicionesLlenas tableroSoloConFichasJugador [1,5,9,10,14] = True
   |posicionesLlenas tableroSoloConFichasJugador [1,5,6,7,11,15] = True
   |posicionesLlenas tableroSoloConFichasJugador [1,5,6,10,11,15] = True
   |posicionesLlenas tableroSoloConFichasJugador [2,6,5,4,8,12] = True
   |posicionesLlenas tableroSoloConFichasJugador [2,6,10,9,8,12] = True
   |posicionesLlenas tableroSoloConFichasJugador [2,6,5,9,8,12] = True
   |posicionesLlenas tableroSoloConFichasJugador [2,6,5,9,13] = True
   |posicionesLlenas tableroSoloConFichasJugador [2,6,10,9,13] = True
   |posicionesLlenas tableroSoloConFichasJugador [2,6,7,11,15] = True
   |posicionesLlenas tableroSoloConFichasJugador [2,6,10,11,15] = True
   |posicionesLlenas tableroSoloConFichasJugador [1,5,9,10,11,15] = True
   |posicionesLlenas tableroSoloConFichasJugador [3,7,6,5,9,13] = True
   |posicionesLlenas tableroSoloConFichasJugador [3,7,11,10,9,13] = True
   |posicionesLlenas tableroSoloConFichasJugador [3,7,6,10,9,13] = True
   |posicionesLlenas tableroSoloConFichasJugador [3,7,6,10,14] = True
   |posicionesLlenas tableroSoloConFichasJugador [3,7,11,10,14] = True
   |posicionesLlenas tableroSoloConFichasJugador [12,13,14,10,11] = True
   |posicionesLlenas tableroSoloConFichasJugador [12,13,9,10,11] = True
   |posicionesLlenas tableroSoloConFichasJugador [12,13,9,5,6,7] = True
   |posicionesLlenas tableroSoloConFichasJugador [12,13,9,10,6,7] = True
   |posicionesLlenas tableroSoloConFichasJugador [12,13,14,10,6,7] = True
   |posicionesLlenas tableroSoloConFichasJugador [8,9,13,14,15] = True
   |posicionesLlenas tableroSoloConFichasJugador [8,9,10,14,15] = True
   |posicionesLlenas tableroSoloConFichasJugador [8,9,5,6,7] = True
   |posicionesLlenas tableroSoloConFichasJugador [8,9,10,6,7] = True
   |posicionesLlenas tableroSoloConFichasJugador [8,9,5,1,2,3] = True
   |posicionesLlenas tableroSoloConFichasJugador [8,9,10,6,2,3] = True
   |posicionesLlenas tableroSoloConFichasJugador [8,9,5,6,2,3] = True
   |posicionesLlenas tableroSoloConFichasJugador [4,5,1,2,3] = True
   |posicionesLlenas tableroSoloConFichasJugador [4,5,6,2,3] = True
   |posicionesLlenas tableroSoloConFichasJugador [4,5,9,10,11] = True
   |posicionesLlenas tableroSoloConFichasJugador [4,5,6,10,11] = True
   |posicionesLlenas tableroSoloConFichasJugador [4,5,9,13,14,15] = True
   |posicionesLlenas tableroSoloConFichasJugador [4,5,9,10,14,15] = True
   |posicionesLlenas tableroSoloConFichasJugador [4,5,6,10,14,15] = True
   |posicionesLlenas tableroSoloConFichasJugador [0,1,2,6,7] = True
   |posicionesLlenas tableroSoloConFichasJugador [0,1,5,6,7] = True
   |posicionesLlenas tableroSoloConFichasJugador [0,1,5,9,10,11] = True
   |posicionesLlenas tableroSoloConFichasJugador [0,1,5,6,10,11] = True
   |posicionesLlenas tableroSoloConFichasJugador [0,1,2,6,10,11] = True
   |otherwise = False
      where 
         tableroSoloConFichasJugador = map (vaciarCasillaSiNoEsDeJugador jugador) tablero

vaciarCasillaSiNoEsDeJugador :: TakPlayer -> Casilla -> Casilla
--vaciar casilla si no es jugador, o es vertical
vaciarCasillaSiNoEsDeJugador jugador casilla = if ok then casilla else (ConstructorCasilla [])
   where
      fichaArriba = fichaDeArriba casilla
      ok = fichaArriba == (Just (Horizontal jugador))
