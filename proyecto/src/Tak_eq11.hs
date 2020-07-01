{- Tak ------------------------------------------------------------------------------------------
 
Plantilla de código para el proyecto del curso de 2020 de _Programación Funcional_ para las carreras 
de Ingeniería y Licenciatura en Informática de la FIT (UCU).
Los docentes no garantizan que este código esté libre de errores. De encontrar problemas, por favor
reportarlos a la cátedra.

Leonardo Val, Ignacio Pacheco.
-}
module Tak where

import Data.Maybe (fromJust, listToMaybe)
import Data.List (elemIndex)
import System.Random --para que cargue bien usar stack ghci

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

data TakPlayer = WhitePlayer | BlackPlayer deriving (Eq, Show, Enum, Bounded)
data TakGame = ConstructorTakGame Tablero TakPlayer deriving (Eq,Show)
data TakAction = Colocar Int Ficha | Mover Int Int Ficha deriving (Eq,Show)

beginning3x3 :: TakGame -- El estado inicial del juego Tak con un tablero de 3x3, con el tablero vacío. 
beginning3x3 = ConstructorTakGame [ ConstructorCasilla [] | _ <- [1..9]] WhitePlayer

beginning4x4 :: TakGame -- El estado inicial del juego Tak con un tablero de 4x4, con el tablero vacío. 
beginning4x4 = ConstructorTakGame [ ConstructorCasilla [] | _ <- [1..16]] WhitePlayer

actions :: TakGame -> [(TakPlayer, [TakAction])] -- La lista debe incluir una y solo una tupla para cada jugador. 
                                                     -- Si el jugador está activo, la lista asociada debe incluir todos sus 
                                                     -- posibles movimientos para el estado de juego dado. Sino la lista debe estar vacía.
actions juego                                    
    |activo==WhitePlayer = [(BlackPlayer,[]) , (WhitePlayer, [Colocar cas fichaBlanca | cas <- posicionesVacias ]  ) ]
    |activo==BlackPlayer = [(BlackPlayer, [Colocar cas fichaNegra | cas <- posicionesVacias]  ) , (WhitePlayer,[]) ]
        where
                activo = activePlayer juego                           
                fichaBlanca = Horizontal WhitePlayer
                fichaNegra = Horizontal BlackPlayer
                posicionesVacias = [y | x <- (getTablero juego) ,  y <- [0..(length (getTablero juego) )]  , casillaVacia x]
actions _ = error "no existis" 

next :: TakGame -> (TakPlayer, TakAction) -> TakGame -- Esta función aplica una acción sobre un estado de juego dado, y retorna 
                                                         -- jugador activo, si el juego está terminado, o si la acción no es realizable. 
next (ConstructorTakGame casillas WhitePlayer) (jugador, (Colocar cas ficha) ) = ConstructorTakGame (colocar casillas (jugador, (Colocar cas ficha) ) ) (BlackPlayer)                                                    -- el estado resultante. Se debe levantar un error si el jugador dado no es el 
next _ _ = error "no implementado"   

result :: TakGame -> [(TakPlayer, Int)]
result (TakGame f) = zip players (if f then [] else [1, -1]) --TODO

score :: TakGame -> [(TakPlayer, Int)]
score _ = zip players [0, 0] --TODO

showBoard :: TakGame -> String
showBoard g = show g --TODO

showAction :: TakAction -> String
showAction a = show a --TODO
   
readAction :: String -> TakAction
readAction = read --TODO

activePlayer :: TakGame -> Maybe TakPlayer
activePlayer g = listToMaybe [p | (p, as) <- actions g, not (null as)]

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
      putStrLn ("Select one move:" ++ concat [" "++ show m | m <- moves])
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