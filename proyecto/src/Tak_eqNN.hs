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
data TakGame = TakGame Bool deriving (Eq, Show) --TODO
data TakAction = TakAction deriving (Eq, Show, Read) --TODO

beginning3x3 :: TakGame
beginning3x3 = TakGame False --TODO

beginning4x4 :: TakGame
beginning4x4 = TakGame False --TODO

actions :: TakGame -> [(TakPlayer, [TakAction])]
actions (TakGame f) = zip players [if f then [] else [TakAction], []] --TODO

next :: TakGame -> (TakPlayer, TakAction) -> TakGame
next _ _ = TakGame True --TODO

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
