import Data.List
import Data.Maybe
import Data.Char

data Ficha = Horizontal TakPlayer | Vertical TakPlayer deriving (Eq,Show)
type Tablero = [Casilla]
data TakPlayer = WhitePlayer | BlackPlayer deriving (Eq, Enum, Bounded)
data TakGame = ConstructorTakGame Tablero TakPlayer deriving (Eq,Show)
data TakAction = Colocar Int Ficha | Mover Int Int [Int] | Invalido deriving (Eq, Show)
data Casilla = ConstructorCasilla [Ficha] deriving (Eq,Show)


instance Show TakPlayer where
   show WhitePlayer = "W"
   show BlackPlayer = "B"

takeHasta :: (Eq a) => [a] -> (a->Bool)-> [a]
--hace un take en la lista desde el principio hasta una posicion antes
--de la primera ocurrencia del primer elemento que cumpla con la condicion en f
takeHasta lista f
   |posicionPrimero == (-1) = lista
   |otherwise = take posicionPrimero lista
   where
      mapAf = map f lista
      posicionPrimero = fromMaybe (-1) (elemIndex True mapAf)

vaciarCasillaSiNoEsDeJugador :: TakPlayer -> Casilla -> Casilla
--vaciar casilla si no es jugador, o es vertical
vaciarCasillaSiNoEsDeJugador jugador casilla = if ok then casilla else (ConstructorCasilla [])
   where
      fichaArriba = fichaDeArriba casilla
      ok = fichaArriba == (Just (Horizontal jugador))

posicionesLlenas :: Tablero -> [Int] -> Bool
posicionesLlenas tablero posiciones = foldr1 (&&) llenas
        where
            llenas = [ not (casillaVacia (tablero!!x)) | x <- posiciones]

fichaDeArriba :: Casilla -> Maybe Ficha
fichaDeArriba (ConstructorCasilla fichas) = if fichas == [] then Nothing else (Just (last fichas))

casillaVacia :: Casilla -> Bool
casillaVacia (ConstructorCasilla [] ) = True
casillaVacia _ = False





caminoCompleto :: Tablero -> TakPlayer -> Bool
caminoCompleto tablero jugador

      --si existiera una funcion magica que tire cada una de estas listas se puede
      --hacer un map o algo asi con todas
      -- funcionMagica :: Tablero -> [[Int]]
      |length tablero == 9 = camino3x3 tablero jugador
      |length tablero == 16 = camino4x4 tablero jugador
      |otherwise = False
         
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