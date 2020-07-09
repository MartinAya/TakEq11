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

coordenadasNxN :: Int -> Path
coordenadasNxN largoTablero = stringIndices
   where
      largoFila = (floor (sqrt (fromIntegral largoTablero)))
      stringIndices = map (\n -> divMod n largoFila ) [0..(largoTablero-1)]

orthDeltas :: [Coord]
orthDeltas = [(x, y) | x <- [-1..1], y <- [-1..1], (x == 0) /= (y == 0)]

insideBoard :: Int -> Coord -> Bool
insideBoard size (x, y) = x >= 0 && y >= 0 && x < size && y < size

orthAdjacent :: Int -> Coord -> [Coord] 
orthAdjacent size (x, y) = filter (insideBoard size) [(x + dx, y + dy) | (dx, dy) <- orthDeltas]

walks :: Int -> [Path] -> [Path]
walks size paths
  -- Punto fijo, la lista de entrada y resultado tiene los mismo valores.
  | null (nextPaths \\ paths) = paths
  -- Se cambiaron los caminos, hay que seguir calculando.
  | otherwise = walks size nextPaths
  where step = [next:path | path <- paths, next <- orthAdjacent size (head path)]
        nextPaths = filter (\p -> (length p) <= 2 * (size - 1)) step

--La función walks se utiliza para calcular una composición de movimientos ortogonales 
--(clausura transitiva) a partir de una lista de posiciones dadas. A este tipo de algoritmos 
--se los conoce como iteraciones de punto fijo o bucles caóticos. La función debería llamarse 
--con las posiciones sobre los bordes superior e izquierdo. Tiene el problema de que retorna 
--caminos más largos de lo necesario, aquellos que se mueven a lo largo de uno de los bordes. 
--Les dejo a ustedes solucionar ese problema.



conectaHorizontal :: Tablero -> Int -> Int -> Bool
conectaHorizontal tablero inicial final 
    |length tablero == 9 = (x0 == 0 && x1 == 2) || (x1 == 0 && x0 == 2)
    |length tablero == 16 = (y0 == 0 && x1 == 3) || (y1 == 0 && y0 == 3)
        where
            (x0,y0) = intANxN tablero inicial
            (x1,y1) = intANxN tablero final
        
conectaVertical :: Tablero -> Int -> Int -> Bool
conectaVertical tablero inicial final  
    |length tablero == 9 = (y0 == 0 && y1 == 2) || (y1 == 0 && y0 == 2)
    |length tablero == 16 = (y0 == 0 && y1 == 3) || (y1 == 0 && y0 == 3)
        where
            (x0,y0) = intANxN tablero inicial
            (x1,y1) = intANxN tablero final

casillaArriba :: Tablero -> Int -> Maybe Int
casillaArriba tablero actual = elemIndex tupla posicionesnxn
   where
      posicionesnxn = coordenadasNxN tablero
      (x,y) = intANxN tablero actual
      tupla = (x+1,y)

casillaDerecha :: Tablero -> Int -> Maybe Int
casillaDerecha tablero actual = elemIndex tupla posicionesnxn
   where
      posicionesnxn = coordenadasNxN tablero
      (x,y) = intANxN tablero actual
      tupla = (x,y+1)
      
caminoHorizontalArriba :: TakPlayer -> Tablero -> Int -> Int -> Bool
--recibe un tablero, una posicion de jugador, y devuevle si exite un camino que conecta hacia el otro lado
caminoHorizontalArriba jugador tablero inical actual
   |conectaHorizontal tablero inicial actual = True --ya llegamos
   |(irParaArriba == irParaDerecha ) == False = False --ya no queda a donde ir
   |irParaArriba == False = caminoHorizontalArriba tablero inicial casillaArriba --se puede ir solo para arriba
   |irParaDerecha == False = caminoHorizontalArriba tablero inical casillaDerecha --se puede ir solo derecha
   |otherwise = (caminoHorizontalArriba tablero inical casillaDerecha) || (caminoHorizontalArriba tablero inicial casillaArriba) --hay que ir para los dos lados
   where
      casillaArriba = casillaArriba Int
      casillaDerecha = casillaDerecha Int
      irParaArriba = if casillaArriba==Nothing then False else (casillaDeJugadorYH jugador tablero casillaArriba)
      irParaDerecha = if casillaDerecha==Nothing then False else (casillaDeJugadorYH jugador tablero casillaDerecha)


casillaDeJugadorYH :: TakPlayer -> Tablero-> Maybe Int -> Bool
casillaDeJugadorYH jugador tablero posicion
   |posicion == Nothing = False
   |otherwise = ok
      where
         casilla = tablero!!(fromMaybe (-1) posicion)
         fichaArriba = fichaDeArriba casilla
         ok = fichaArriba == (Just (Horizontal jugador))