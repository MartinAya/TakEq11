{- Tak ------------------------------------------------------------------------------------------
 
Plantilla de código para el proyecto del curso de 2020 de _Programación Funcional_ para las carreras 
de Ingeniería y Licenciatura en Informática de la FIT (UCU).
Los docentes no garantizan que este código esté libre de errores. De encontrar problemas, por favor
reportarlos a la cátedra.

Leonardo Val, Ignacio Pacheco.
-}
module Tak where

import Data.Maybe
import Data.List (elemIndex)
import System.Random --para que cargue bien usar stack ghci
import Data.List.Split

{- Es posible que el paquete `System.Random` no esté disponible si se instaló el core de la Haskell 
Platform en el sistema. Para instalarlo, ejecutar los siguientes comandos:

> cabal update
> cabal install random

La herramienta `cabal` es un manejador de paquetes usado por la plataforma Haskell. Debería estar 
disponible junto con el `ghci`.

-}

{-- Lógica de juego --------------------------------------------------------------------------------

Funciones de marca sin ninguna implementación útil. Reemplazar por el código apropiado o por imports
a los módulos necesarios.
-}

data Ficha = Horizontal TakPlayer | Vertical TakPlayer deriving (Eq,Show)
type Tablero = [Casilla]
data TakPlayer = WhitePlayer | BlackPlayer deriving (Eq, Enum, Bounded)
data TakGame = ConstructorTakGame Tablero TakPlayer deriving (Eq,Show)
data TakAction = Colocar Int Ficha | Mover Int Int Ficha deriving (Eq, Show)
data Casilla = ConstructorCasilla [Ficha] deriving (Eq,Show)


instance Show TakPlayer where
   show WhitePlayer = "W"
   show BlackPlayer = "B"


beginning3x3 :: TakGame -- El estado inicial del juego Tak con un tablero de 3x3, con el tablero vacío. 
beginning3x3 = ConstructorTakGame [ ConstructorCasilla [] | _ <- [1..9]] WhitePlayer

beginning4x4 :: TakGame -- El estado inicial del juego Tak con un tablero de 4x4, con el tablero vacío. 
beginning4x4 = ConstructorTakGame [ ConstructorCasilla [] | _ <- [1..16]] WhitePlayer

-- La lista debe incluir una y solo una tupla para cada jugador. 
-- Si el jugador está activo, la lista asociada debe incluir todos sus 
-- posibles movimientos para el estado de juego dado. Sino la lista debe estar vacía.
actions :: TakGame -> [(TakPlayer, [TakAction])]
--actions juego = [(BlackPlayer,[]) , (WhitePlayer, [Colocar cas fichaBlanca | cas <- posicionesVacias ]  ) ]
actions (ConstructorTakGame tablero activo)                                    
      |activo==WhitePlayer = [(BlackPlayer,[]) , (WhitePlayer, [Colocar cas fichaBlanca | cas <- posicionesVacias ]  ) ]
      |activo== BlackPlayer = [(BlackPlayer, [Colocar cas fichaNegra | cas <- posicionesVacias]  ) , (WhitePlayer,[]) ]
        where      
            fichaBlanca = Horizontal WhitePlayer
            fichaNegra = Horizontal BlackPlayer
            posicionesVacias = [    x| x <- [0..(length tablero - 1)], casillaVacia (tablero!!x)]
actions _ = error "actions: error" 

next :: TakGame -> (TakPlayer, TakAction) -> TakGame -- Esta función aplica una acción sobre un estado de juego dado, y retorna 
                                                         -- jugador activo, si el juego está terminado, o si la acción no es realizable. 
next (ConstructorTakGame casillas WhitePlayer) (jugador, (Colocar cas ficha) ) = ConstructorTakGame (colocar casillas (jugador, (Colocar cas ficha) ) ) (BlackPlayer)    
next (ConstructorTakGame casillas BlackPlayer) (jugador, (Colocar cas ficha) ) = ConstructorTakGame (colocar casillas (jugador, (Colocar cas ficha) ) ) (WhitePlayer)                                                
next _ _ = error "next: no implementado" -- el estado resultante. Se debe levantar un error si el jugador dado no es el 

casillaVacia :: Casilla -> Bool
casillaVacia (ConstructorCasilla [] ) = True
casillaVacia _ = False

getTablero :: TakGame -> Tablero
getTablero (ConstructorTakGame tablero _) = tablero
    
setLista :: (Eq a) => [a] -> Int -> a -> [a]
setLista lista posicion ele
   |posicion < 0 || posicion > (length lista - 1)= error "indice invalido"
   |True = [if x==posicion then ele else lista!!x | x <- [0..(length lista-1)] ]

result :: TakGame -> [(TakPlayer, Int)]
result (ConstructorTakGame _ _) = zip players (if True then [] else [1, -1]) --TODO

--score :: TakGame -> [(TakPlayer, Int)]
--score _ = zip players [0, 0] --TODO


coordenadas3X3 :: [(Int,Int)] 
coordenadas3X3 = map (\n -> divMod n 3) [0..8]

intA3x3 :: Int -> (Int,Int)
intA3x3 posicion =  coords !! posicion 
   where
      coords = coordenadas3X3

tx3Aint :: (Int,Int) -> Int
tx3Aint tupla = if (resultado==Nothing) 
   then error "coordenadas no válidas" 
   else fromMaybe 0 resultado
      --Nothing
      --Just 8
      where resultado = elemIndex tupla coordenadas3X3
      
showAction :: TakAction -> String
showAction (Colocar int ficha) = "C " ++ show (intA3x3 int) ++" "++ showFicha (ficha)
   
readAction :: String -> TakAction
readAction entrada
   |accion =="C" && orientacion=="H" =  Colocar posicion (Horizontal juga)
   where
      --caso colocar: C,0,1,H,W
      [accion,ind1,ind2,orientacion,jugador] = splitOn "," entrada
      posicion = tx3Aint (read (ind1), read (ind2))
      juga = if jugador=="W" then WhitePlayer else if jugador=="B" then BlackPlayer else error "jugador no valido"
readAction _ = error "accion no valida o no implementada"
   
      

-- activePlayer :: TakGame -> Maybe TakPlayer
-- activePlayer g = listToMaybe [p | (p, as) <- actions g, not (null as)]

-- activePlayer :: TakGame -> TakPlayer -- Esta función determina a cuál jugador le toca mover, dado un estado de juego.
-- activePlayer (ConstructorTakGame _ WhitePlayer) = WhitePlayer
-- activePlayer (ConstructorTakGame _ BlackPlayer) = BlackPlayer

activePlayer :: TakGame -> Maybe TakPlayer
activePlayer g = listToMaybe [p | (p, as) <- actions g, not (null as)]



apilarFicha :: Casilla -> Ficha -> Casilla
apilarFicha (ConstructorCasilla fichas) fichaNueva = (ConstructorCasilla (fichaNueva:(fichas)))

colocar :: Tablero -> (TakPlayer, TakAction) -> Tablero
colocar tablero (WhitePlayer, (Colocar posicion fichaBlanca)) = nuevoTablero
      where
            nuevoTablero = setLista tablero (posicion) nuevaCasilla
            laCasilla = tablero!!posicion
            nuevaCasilla = apilarFicha laCasilla (Horizontal WhitePlayer)
colocar tablero (BlackPlayer, (Colocar posicion fichaNegra)) = nuevoTablero
      where
            nuevoTablero = setLista tablero (posicion) nuevaCasilla
            laCasilla = tablero!!posicion
            nuevaCasilla = apilarFicha laCasilla (Horizontal BlackPlayer)
colocar _ _ = error "no implementado"

tableroLleno :: Tablero -> Bool
tableroLleno tablero = length (filter casillaVacia tablero) == 0

fichaDeArriba :: Casilla -> Ficha
fichaDeArriba (ConstructorCasilla fichas) = last fichas

getJugadorEnFicha :: Ficha -> TakPlayer
getJugadorEnFicha (Horizontal jug) = jug
getJugadorEnFicha (Vertical jug) = jug


casillaDeJugador :: TakPlayer -> Casilla -> Bool
casillaDeJugador jugador casilla = jugador == jug
        where
            jug = getJugadorEnFicha (fichaDeArriba casilla)
        
posicionesLlenas :: Tablero -> [Int] -> Bool
posicionesLlenas tablero posiciones = foldr1 (&&) llenas
        where
            llenas = [ not (casillaVacia (tablero!!x)) | x <- posiciones]

caminoCompleto :: Tablero -> TakPlayer -> Bool
caminoCompleto tablero jugador

        --si existiera una funcion magica que tire cada una de estas listas se puede
        --hacer un map o algo asi con todas
        -- funcionMagica :: Tablero -> [[Int]]
        |posicionesLlenas tableroSoloConFichasJugador [1,4,7] = True
        |posicionesLlenas tableroSoloConFichasJugador [2,5,8] = True
        |posicionesLlenas tableroSoloConFichasJugador [3,6,9] = True

        |posicionesLlenas tableroSoloConFichasJugador [1,2,3] = True
        |posicionesLlenas tableroSoloConFichasJugador [4,5,6] = True
        |posicionesLlenas tableroSoloConFichasJugador [7,8,9] = True

        |posicionesLlenas tableroSoloConFichasJugador [1,4,5,8] = True
        |posicionesLlenas tableroSoloConFichasJugador [2,5,6,9] = True
        |posicionesLlenas tableroSoloConFichasJugador [3,6,5,8] = True
        |posicionesLlenas tableroSoloConFichasJugador [2,5,4,7] = True
        |posicionesLlenas tableroSoloConFichasJugador [1,2,5,6] = True
        |posicionesLlenas tableroSoloConFichasJugador [4,5,8,9] = True
        |posicionesLlenas tableroSoloConFichasJugador [7,8,5,6] = True
        |posicionesLlenas tableroSoloConFichasJugador [4,5,2,3] = True
        |True  = False
        where
            tableroSoloConFichasJugador = map (vaciarCasillaSiNoEsDeJugador jugador) tablero

vaciarCasillaSiNoEsDeJugador :: TakPlayer -> Casilla -> Casilla
vaciarCasillaSiNoEsDeJugador jugador casilla
        |casillaDeJugador jugador casilla = casilla
        |True = ConstructorCasilla []

showFicha :: Ficha -> String
showFicha (Horizontal ply) = " H" ++ "-" ++show (ply) 
showFicha (Vertical ply) = " V" ++ "-" ++show (ply) 


--putStr (showGame beginning3x3)
showCasilla :: Casilla -> String
showCasilla (ConstructorCasilla []) = " vacia" ++ "\n"
showCasilla (ConstructorCasilla fichas) = (foldr1 (++) (map showFicha fichas)) ++ "\n"
    

showTablero :: Tablero -> String
showTablero tablero = foldr1 (++) (indiceYCasilla)
        where
            stringCasillas = map showCasilla tablero
            stringIndices = map (\n -> divMod n 3) [0..8]
            indiceYCasilla = zipWith (++) (map show stringIndices) stringCasillas

showBoard :: TakGame -> String -- Convierte el estado de juego a un texto que puede ser impreso en la consola para mostrar el tablero y demás información de la partida. 
showBoard (ConstructorTakGame tablero WhitePlayer) = "Le toca a Blancas " ++ "\n" ++  (showTablero tablero)
showBoard (ConstructorTakGame tablero BlackPlayer) = "Le toca a Negras "++ "\n" ++ (showTablero tablero)
    


players :: [TakPlayer]
players = [minBound..maxBound]

{-- Match controller -------------------------------------------------------------------------------

Código de prueba. Incluye una función para correr las partidas y dos agentes: consola y aleatorio.

-}
type TakAgent = TakGame -> IO (Maybe TakAction)

{- La función ´runMatch´ corre la partida completa a partir del estado de juego dado, usando los dos 
agentes dados. Retorna una tupla con los puntajes (score) finales del juego.
-}
runMatch :: (TakAgent, TakAgent) -> TakGame -> IO [(TakPlayer, Int)]
runMatch ags@(ag1, ag2) g = do
   putStrLn (showBoard g)
   case (activePlayer g) of
      Nothing -> return $ result g
      Just p -> do
         let ag = [ag1, ag2] !! (fromJust (elemIndex p players))
         move <- ag g
         runMatch ags (Tak.next g (p, fromJust move))

{- La función ´runOnConsole´ ejecuta toda la partida a partir del estado inicial usando dos agentes
de consola.
-}
runOnConsole :: TakGame -> IO [(TakPlayer, Int)]
runOnConsole g = do
   runMatch (consoleAgent WhitePlayer, consoleAgent BlackPlayer) g

run3x3OnConsole :: IO [(TakPlayer, Int)]
run3x3OnConsole = runOnConsole beginning3x3

run4x4OnConsole :: IO [(TakPlayer, Int)]
run4x4OnConsole = runOnConsole beginning4x4


{- El agente de consola ´consoleAgent´ muestra el estado de juego y los movimientos disponibles por
consola, y espera una acción por entrada de texto.
-}
consoleAgent :: TakPlayer -> TakAgent
consoleAgent player state = do
   let moves = fromJust (lookup player (actions state))
   if null moves then do
      putStrLn "No moves!"
      getLine
      return Nothing
   else do
      putStrLn ("Select one move:" ++ concat ["\n"++ showAction m | m <- moves])--se cambio show por showAction
      line <- getLine
      let input = readAction line
      if elem input moves then return (Just input) else do 
         putStrLn "Invalid move!"
         consoleAgent player state

randomAgent :: TakPlayer -> TakAgent
randomAgent player state = do
    let moves = fromJust (lookup player (actions state))
    if null moves then do
       putStrLn "No moves!"
       return Nothing
    else do
       i <- randomRIO (0, (length moves) - 1)
       return (Just (moves !! i))



-- Fin