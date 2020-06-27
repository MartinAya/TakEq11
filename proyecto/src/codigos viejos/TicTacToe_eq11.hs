{- TicTacToe ---------------------------------------------------------------------------------------
 
Plantilla de código para el trabajo de aplicación de la UT2 del curso de 2020 de 
_Programación Funcional_ para las carreras de Ingeniería y Licenciatura en Informática de la FIT 
(UCU).
Los docentes no garantizan que este código esté libre de errores. De encontrar problemas, por favor
reportarlos a la cátedra.

Leonardo Val, Ignacio Pacheco.
-}
module TicTacToe_eq11 where

import Data.Maybe (fromJust, listToMaybe)
import Data.List (elemIndex)
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

type TicTacToePlayer = String
playerX = "X"
playerO = "O"
noPlayer = "-"

type TicTacToeAction = Int


type TicTacToeGame = [TicTacToePlayer]

beginning::TicTacToeGame
beginning=["-","-","-","-","-","-","-","-","-"]


activePlayer :: TicTacToeGame -> TicTacToePlayer
activePlayer g = if(contarInstancias g "X") > (contarInstancias g "O") then playerO else playerX

--actions :: TicTacToeGame -> [(TicTacToePlayer, [TicTacToeAction])]
--actions g = zip players [if g then [] else [False], []] --TODO

contarInstancias :: (Eq a) => [a] -> a -> Int
contarInstancias [] a = 0
contarInstancias (x:[]) a
    |x==a = 1
    |otherwise = 0
contarInstancias (x:xs) a
    |x == a = 1 + contarInstancias(xs) a
    |otherwise = 0 + contarInstancias(xs) a
    

next :: TicTacToeGame -> (TicTacToePlayer,TicTacToeAction)-> TicTacToeGame
next tablero (jugador, posicion)
   |(tablero !! posicion == "-") = setLista tablero posicion jugador
   |otherwise = tablero

setLista :: (Eq a) => [a] -> Int -> a -> [a]
setLista lista lugar elem = subLista lista 0 (lugar-1) ++ [elem] ++  subLista lista (lugar+1) (length lista)

subLista :: (Eq a) => [a] -> Int -> Int -> [a]
subLista lista desde hasta
   |desde == hasta = [lista!!hasta]
   |desde > hasta = error "indices invalidos"
   |otherwise = take (hasta-desde+1) (drop (desde) lista)

actions :: TicTacToeGame -> [(TicTacToePlayer, [TicTacToeAction])]
actions g
   | activePlayer g == playerX = [(playerX, posiblesMovimientos g 0), (playerO, [])]
   | otherwise = [(playerX, []), (playerO, posiblesMovimientos g 0)]
   
posiblesMovimientos :: TicTacToeGame -> Int -> [TicTacToeAction]
posiblesMovimientos [] _ = []
posiblesMovimientos (g:gs) numero
   | g == noPlayer = numero:posiblesMovimientos gs (numero + 1)
   | otherwise = posiblesMovimientos gs (numero + 1)
   
termino:: TicTacToeGame -> Bool
termino lista
   --Horizontales Ganadoras
   |lista!!0 == lista!!1 && lista!!1 == lista!!2 && lista!!0 /= "-" = True
   |lista!!3 == lista!!4 && lista!!4 == lista!!5 && lista!!3 /= "-" = True
   |lista!!6 == lista!!7 && lista!!8 == lista!!8 && lista!!6 /= "-" = True
   --Verticales Ganadoras
   |lista!!0 == lista!!3 && lista!!3 == lista!!6 && lista!!0 /= "-" = True
   |lista!!1 == lista!!4 && lista!!4 == lista!!7 && lista!!1 /= "-" = True
   |lista!!2 == lista!!5 && lista!!5 == lista!!8 && lista!!2 /= "-" = True
   --Diagonales Ganadoras
   |lista!!0 == lista!!4 && lista!!4 == lista!!8 && lista!!0 /= "-" = True
   |lista!!2 == lista!!4 && lista!!4 == lista!!6 && lista!!2 /= "-" = True
   --Caso de Empate
   |not(elem "-" lista) = True
   --NO ha ternimado el juego
   |otherwise = False

result :: TicTacToeGame -> [(TicTacToePlayer, Int)]
result lista
   --Horizontales Ganadoras
   |lista!!0 == lista!!1 && lista!!1 == lista!!2 && lista!!0 == "X" = [(playerX,1),(playerO,(-1))]
   |lista!!3 == lista!!4 && lista!!4 == lista!!5 && lista!!3 == "X" = [(playerX,1),(playerO,(-1))]
   |lista!!6 == lista!!7 && lista!!8 == lista!!8 && lista!!6 == "X" = [(playerX,1),(playerO,(-1))]
   --Verticales Ganadoras
   |lista!!0 == lista!!3 && lista!!3 == lista!!6 && lista!!0 == "X" = [(playerX,1),(playerO,(-1))]
   |lista!!1 == lista!!4 && lista!!4 == lista!!7 && lista!!1 == "X" = [(playerX,1),(playerO,(-1))]
   |lista!!2 == lista!!5 && lista!!5 == lista!!8 && lista!!2 == "X" = [(playerX,1),(playerO,(-1))]
   --Diagonales Ganadoras
   |lista!!0 == lista!!4 && lista!!4 == lista!!8 && lista!!0 == "X" = [(playerX,1),(playerO,(-1))]
   |lista!!2 == lista!!4 && lista!!4 == lista!!6 && lista!!2 == "X" = [(playerX,1),(playerO,(-1))]
    
   --Horizontales Ganadoras
   |lista!!0 == lista!!1 && lista!!1 == lista!!2 && lista!!0 == "O" = [(playerX,(-1)),(playerO,1)]
   |lista!!3 == lista!!4 && lista!!4 == lista!!5 && lista!!3 == "O" = [(playerX,(-1)),(playerO,1)]
   |lista!!6 == lista!!7 && lista!!8 == lista!!8 && lista!!6 == "O" = [(playerX,(-1)),(playerO,1)]
   --Verticales Ganadoras
   |lista!!0 == lista!!3 && lista!!3 == lista!!6 && lista!!0 == "O" = [(playerX,(-1)),(playerO,1)]
   |lista!!1 == lista!!4 && lista!!4 == lista!!7 && lista!!1 == "O" = [(playerX,(-1)),(playerO,1)]
   |lista!!2 == lista!!5 && lista!!5 == lista!!8 && lista!!2 == "O" = [(playerX,(-1)),(playerO,1)]
   --Diagonales Ganadoras
   |lista!!0 == lista!!4 && lista!!4 == lista!!8 && lista!!0 == "O" = [(playerX,(-1)),(playerO,1)]
   |lista!!2 == lista!!4 && lista!!4 == lista!!6 && lista!!2 == "O" = [(playerX,(-1)),(playerO,1)]
   --Caso de Empate
   |otherwise = [(playerX,0),(playerO,0)]


showBoard :: TicTacToeGame -> String
showBoard g = show g --TODO

showAction :: TicTacToeAction -> String
showAction a = show a --TODO
   
readAction :: String -> TicTacToeAction
readAction cadena = read cadena


players :: [TicTacToePlayer]
players = [playerX, playerO]

{-- Match controller -------------------------------------------------------------------------------

Código de prueba. Incluye una función para correr las partidas y dos agentes: consola y aleatorio.

-}
type TicTacToeAgent = TicTacToeGame -> IO (Maybe TicTacToeAction)

{- La función ´runMatch´ corre la partida completa a partir del estado de juego dado, usando los dos 
agentes dados. Retorna una tupla con los puntajes (score) finales del juego.
-}
runMatch :: (TicTacToeAgent, TicTacToeAgent) -> TicTacToeGame -> IO [(TicTacToePlayer, Int)]
runMatch ags@(ag1, ag2) g = do
   putStrLn (showBoard g)
   let p = activePlayer g
   if p == noPlayer then return (result g)
   else do
     let ag = [ag1, ag2] !! (fromJust (elemIndex p players))
     move <- ag g
     runMatch ags (next g (p, fromJust move))


{- La función ´runOnConsole´ ejecuta toda la partida a partir del estado inicial usando dos agentes
de consola.
-}
runOnConsole :: IO [(TicTacToePlayer, Int)]
runOnConsole = do
   runMatch (consoleAgent playerX, consoleAgent playerO) beginning

{- El agente de consola ´consoleAgent´ muestra el estado de juego y los movimientos disponibles por
consola, y espera una acción por entrada de texto.
-}
consoleAgent :: TicTacToePlayer -> TicTacToeAgent
consoleAgent player state = do
   let moves = fromJust $ lookup player (actions state)
   if null moves then do
      putStrLn "No moves!"
      getLine
      return Nothing
   else do
      putStrLn ("Select one move:" ++ concat [" "++ showAction m | m <- moves])
      line <- getLine
      let input = readAction line
      if elem input moves then return (Just input) else do 
         putStrLn "Invalid move!"
         consoleAgent player state

