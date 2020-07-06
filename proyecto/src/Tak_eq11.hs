{- Tak ------------------------------------------------------------------------------------------
 
Plantilla de código para el proyecto del curso de 2020 de _Programación Funcional_ para las carreras 
de Ingeniería y Licenciatura en Informática de la FIT (UCU).
Los docentes no garantizan que este código esté libre de errores. De encontrar problemas, por favor
reportarlos a la cátedra.

Leonardo Val, Ignacio Pacheco.
-}
module Tak where

import Data.Maybe
import Data.List
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
data TakAction = Colocar Int Ficha | Mover Int Int [Int] deriving (Eq, Show)
data Casilla = ConstructorCasilla [Ficha] deriving (Eq,Show)


instance Show TakPlayer where
   show WhitePlayer = "W"
   show BlackPlayer = "B"


beginning3x3 :: TakGame -- El estado inicial del juego Tak con un tablero de 3x3, con el tablero vacío. 
beginning3x3 = ConstructorTakGame [ ConstructorCasilla [] | _ <- [1..9]] WhitePlayer

beginning4x4 :: TakGame -- El estado inicial del juego Tak con un tablero de 4x4, con el tablero vacío. 
beginning4x4 = ConstructorTakGame [ ConstructorCasilla [] | _ <- [1..16]] WhitePlayer

actions :: TakGame -> [(TakPlayer, [TakAction])]
actions g@(ConstructorTakGame tablero activo)
   |activo == WhitePlayer = [(BlackPlayer,[]),(WhitePlayer,posiblesColocar++posiblesMover2)]
   |activo == BlackPlayer  = [(WhitePlayer,[]),(BlackPlayer,posiblesColocar++posiblesMover2)]
   --mover :: Tablero -> (TakPlayer, TakAction) -> Tablero   TakAction Mover Int Int [Int]   
   --colocar :: Tablero -> (TakPlayer, TakAction) -> Tablero TakAction Colocar Int Ficha
      where
         posiblesColocar = [Colocar cas (Horizontal activo) | cas <- posicionesVacias ] 
         posicionesVacias = [ x | x <- [0..(length tablero - 1)], casillaVacia (tablero!!x)]
         listaDesdes = posicionesDeJugador g
         desdesYHastas = foldr (++) [] (map calculadorHacia listaDesdes)
         posiblesMover1 = [ (posiblesMoverEnTupla desde hasta tablero) | (desde,hasta) <- desdesYHastas]
         posiblesMover2 = foldr (++) [] posiblesMover1
actions _ = error "actions: error"

posiblesMoverEnTupla :: Int -> Int -> Tablero -> [TakAction]
posiblesMoverEnTupla desde hacia tablero = posiblesMover
   where 
      hastaCuantasFichas = take fichasActuales (cuantasFichasPuedeMover tablero)
      (ConstructorCasilla fichas) = (tablero!!desde)
      fichasActuales = length fichas
      --posiblesApilamientos :: Int -> Int -> Int -> [[Int]]
      apilamientos = foldr1 (++) [ posiblesApilamientos cuantas desde hacia | cuantas <- hastaCuantasFichas ]
      posiblesMover = [ Mover desde hacia a | a <- apilamientos]

--Eliminia dupliados
distintos:: [Int] -> [Int]
distintos [] = []
distintos[a] = [a]
distintos (x:xs)
   |elem x xs = distintos xs
   |otherwise = [x] ++ distintos xs

cuantasFichasPuedeMover :: Tablero -> [Int]
cuantasFichasPuedeMover tablero
   |length tablero == 9 = [1,2,3]
   |length tablero == 16 = [1,2,3,4]

distancia :: (Int,Int) -> (Int,Int) -> Int
--solo devuelve distancia para casillas en misma fila o columna
distancia (x1,y1) (x2,y2)
   |x1==x2 = y2 - y1
   |y1==y2 = x2 - x1

posiblesApilamientos :: Int -> Int -> Int -> [[Int]]
-- cuantas, desde, hasta
posiblesApilamientos cuantas desde hasta = apilamientosValidos
   where
      -- (0,0) (0,1) = 1 - 0 (0,0) (0,2) = 2 - 0 
      -- filaOColumnaEnComun :: (Int,Int) -> (Int,Int) -> [(Int,Int)]
      espacioEntreDesdeYHasta = distancia (intA3x3 desde) (intA3x3 hasta)
      todosPosiblesApilamientos = choose espacioEntreDesdeYHasta [0..cuantas]
      apilamientosValidos = filter (filtrado cuantas) todosPosiblesApilamientos
      
filtrado :: Int -> [Int] -> Bool
filtrado cuantas lista = (sinCerosALaIzquierda lista) && (foldr1 (+) lista == cuantas)
--1 - la suma tiene que ser cuantas
--2 - no puede haber un cero a la izquierda 

sinCerosALaIzquierda :: [Int] -> Bool
sinCerosALaIzquierda [x] = True
sinCerosALaIzquierda (x:y:xs)
   |x==0 && y/=0 = False
   |True = sinCerosALaIzquierda (y:xs)


--https://stackoverflow.com/questions/35118659/haskell-permutations-with-the-length-of-the-output-list
choose n list = concatMap permutations $ choose' list [] where
  choose' []     r = if length r == n then [r] else []
  choose' (x:xs) r | length r == n = [r]
                   | otherwise     = choose' xs (x:r) 
                                  ++ choose' xs r

calculadorHacia :: Int -> [(Int,Int)]
--tiene que tirar tuplas con desde y cada posible hacia
calculadorHacia desde = resultado
   where
      hacias = direccionesPosibles desde
      resultado = [(desde,x) | x <- hacias]
   
posicionesDeJugador :: TakGame -> [Int]
posicionesDeJugador (ConstructorTakGame tablero activo) = indicesPosiciones
      where
         indicesPosiciones = findIndices (casillaDeJugador activo) tablero

masLejano :: Int -> [Int] -> Int
masLejano origen posibles = elMasLejano
   where
      origen3x3 = intA3x3 origen
      posibles3x3 = map intA3x3 posibles
      distancias = map (distancia origen3x3) posibles3x3
      mayorDistancia = maximum distancias
      posicionDelMayor = fromMaybe 0 (elemIndex mayorDistancia distancias)
      elMasLejano = posibles!!posicionDelMayor

direccionesPosibles :: Int -> [Int]   
direccionesPosibles casilla = derecha2++izquierda2++arriba2++abajo2
   where
      (x0,y0) = intA3x3 casilla
      derecha =  map tx3Aint (filter (\(x,y) -> x == x0 && y > y0) coordenadas3X3)
      izquierda =  map tx3Aint (filter (\(x,y) -> x == x0 && y < y0) coordenadas3X3)
      arriba =  map tx3Aint (filter (\(x,y) -> x < x0 && y == y0) coordenadas3X3)
      abajo = map tx3Aint (filter (\(x,y) -> x > x0 && y == y0) coordenadas3X3)
      derecha2 = if derecha /= [] then [(masLejano casilla derecha)] else []
      izquierda2 = if izquierda /= [] then [(masLejano casilla izquierda)] else []
      arriba2 = if arriba /= [] then [(masLejano casilla arriba)] else []
      abajo2 = if abajo /= [] then [(masLejano casilla abajo)] else []
      --en cada guarda que se quede con el que esta mas lejos
      --distancia (intA3x3 desde) (intA3x3 hasta)


next :: TakGame -> (TakPlayer, TakAction) -> TakGame -- Esta función aplica una acción sobre un estado de juego dado, y retorna 
                                                         -- jugador activo, si el juego está terminado, o si la acción no es realizable. 
next (ConstructorTakGame casillas WhitePlayer) (jugador, (Colocar cas ficha) ) = ConstructorTakGame (colocar casillas (jugador, (Colocar cas ficha) ) ) (BlackPlayer)    
next (ConstructorTakGame casillas BlackPlayer) (jugador, (Colocar cas ficha) ) = ConstructorTakGame (colocar casillas (jugador, (Colocar cas ficha) ) ) (WhitePlayer)                                                
next _ _ = error "next: no implementado"



casillaVacia :: Casilla -> Bool
casillaVacia (ConstructorCasilla [] ) = True
casillaVacia _ = False

getTablero :: TakGame -> Tablero
getTablero (ConstructorTakGame tablero _) = tablero
    
setLista :: (Eq a) => [a] -> Int -> a -> [a]
setLista lista posicion ele
   |posicion < 0 || posicion > (length lista - 1)= error "indice invalido"
   |True = [if x==posicion then ele else lista!!x | x <- [0..(length lista-1)] ]

-- El score se calcula como la cantidad de fichas no jugadas por cada jugador.

nFichasXJugador :: Tablero -> Int
nFichasXJugador tablero
   |casillas == 9 = 10
   |casillas == 16 = 15
   |casillas == 25 = 21
   |casillas == 36 = 30
   where
      casillas = length tablero

jugadorEnFicha :: TakPlayer -> Ficha -> Bool
jugadorEnFicha jug (Horizontal jugador) = jug == jugador 
jugadorEnFicha jug (Vertical jugador) = jug == jugador


fichasDeJugadorCasilla :: TakPlayer -> Casilla -> Int
fichasDeJugadorCasilla jugador (ConstructorCasilla fichas) = length (filter (jugadorEnFicha jugador) fichas)


fichasDeJugador :: Tablero -> TakPlayer -> Int
fichasDeJugador tablero jugador = foldr1 (+) (map (fichasDeJugadorCasilla jugador) tablero)
      

score :: TakGame -> [(TakPlayer, Int)]
score (ConstructorTakGame tablero _) = zip players [scoreBlancas, scoreNegras]
   where
      fichasTotales = nFichasXJugador tablero
      scoreBlancas = fichasTotales - (fichasDeJugador tablero WhitePlayer)
      scoreNegras = fichasTotales - (fichasDeJugador tablero BlackPlayer)

result :: TakGame -> [(TakPlayer, Int)]
-- 1 es que ganó, (-1) perdió, 0 empató
-- se asume que cuando se llama a result, el juego ya terminó
-- result evalua el estado actual para decir quien gano, o tirar un score
result g@(ConstructorTakGame tablero jugadorActual)
   |caminoCompletoWhite = [(WhitePlayer,1),(BlackPlayer,(-1))]
   |caminoCompletoBlack = [(WhitePlayer,(-1)),(BlackPlayer,(1))]
   |tablerollen = [(WhitePlayer,0),(BlackPlayer,(0))]
      where
         caminoCompletoWhite = caminoCompleto tablero WhitePlayer
         caminoCompletoBlack = caminoCompleto tablero BlackPlayer
         tablerollen = tableroLleno tablero

ganador :: [(TakPlayer, Int)] -> Maybe TakPlayer
ganador [(WhitePlayer,1),(BlackPlayer,(-1))] = Just WhitePlayer
ganador [(WhitePlayer,(-1)),(BlackPlayer,1)] = Just BlackPlayer
ganador [(WhitePlayer,(0)),(BlackPlayer,0)] = Nothing

agregarPuntajeTablero :: Maybe TakPlayer -> Int -> [(TakPlayer, Int)] -> [(TakPlayer, Int)]
agregarPuntajeTablero Nothing puntajeTablero puntaje = puntaje
agregarPuntajeTablero (Just WhitePlayer) puntajeTablero [(WhitePlayer,p1),(BlackPlayer,p2)] = [(WhitePlayer,p1+puntajeTablero),(BlackPlayer,p2)]
agregarPuntajeTablero (Just BlackPlayer) puntajeTablero [(WhitePlayer,p1),(BlackPlayer,p2)] = [(WhitePlayer,p1),(BlackPlayer,p2+puntajeTablero)]

scoreYResult :: TakGame -> [(TakPlayer, Int)]
scoreYResult game@(ConstructorTakGame tablero _) = resultado ++ puntajeConTablero 
   where 
      resultado = result game
      elGanador = ganador resultado
      puntajeSinTablero = score game 
      puntajeTablero = length tablero
      puntajeConTablero = agregarPuntajeTablero elGanador puntajeTablero puntajeSinTablero

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
showAction (Mover desde direccion apilamiento) = "M " ++ show (intA3x3 desde) ++" "++ show (intA3x3 direccion) ++ (show apilamiento)

readAction :: String -> TakAction
readAction entrada
   |accion =="C" && orientacion=="H" =  Colocar posicion (Horizontal juga)
   where
      --caso colocar: C,0,1,H,W
      [accion,ind1,ind2,orientacion,jugador] = splitOn "," entrada
      posicion = tx3Aint (read (ind1), read (ind2))
      juga = if jugador=="W" then WhitePlayer else if jugador=="B" then BlackPlayer else error "jugador no valido"
      --ejemplo de que quiero mover 3 fichas en la posicion 0,0 hacia la 0,2
      -- M, (0,0) , (0,2) , [1,2]
      --ejemplo de que quiero mover 2 fichas en la posicion 0,0 hacia la 0,2
      -- M, (0,0) , (0,2) , [1,1]
      --ejemplo de que quiero mover 1 fichas en la posicion 0,0 hacia la 0,2
      --M, (0,0) , (0,2) , [1,0]
      --coordenas de la ficha,
      --direccion (coordenas de la casilla mas lejana en esa direccion)
      --lista con cunatas quiere dejar en cada posicion
readAction _ = error "accion no valida o no implementada"
   

-- activePlayer :: TakGame -> Maybe TakPlayer
-- activePlayer g = listToMaybe [p | (p, as) <- actions g, not (null as)]

-- activePlayer :: TakGame -> TakPlayer -- Esta función determina a cuál jugador le toca mover, dado un estado de juego.
-- activePlayer (ConstructorTakGame _ WhitePlayer) = WhitePlayer
-- activePlayer (ConstructorTakGame _ BlackPlayer) = BlackPlayer

activePlayer :: TakGame -> Maybe TakPlayer
--activePlayer tiene que verificar si alguien completo un camino, y si es así, devolver Nothing
activePlayer g@(ConstructorTakGame tablero _)
   |(caminoCompleto tablero WhitePlayer) || (caminoCompleto tablero BlackPlayer) = Nothing
   |True = listToMaybe [p | (p, as) <- actions g, not (null as)]

--el caso en el que se lleno el tablero esta contemplado aca:
--The listToMaybe function returns Nothing on an empty
--list or Just a where a is the first element
--of the list

desapilarDeCasilla :: Casilla -> Int -> ([Ficha],Casilla)
--devuelve las fichas desapiladas en una lista, y el nuevo estado de la casilla
desapilarDeCasilla (ConstructorCasilla fichas) cuantas = (desapiladas,casillaNueva)
      where
         desapiladas = drop ((length fichas) - cuantas) fichas
         nuevaPilaEnCasilla = take ((length fichas) - cuantas) fichas
         casillaNueva = (ConstructorCasilla nuevaPilaEnCasilla)

filaOColumnaEnComun :: (Int,Int) -> (Int,Int) -> [(Int,Int)]
filaOColumnaEnComun (x1,y1) (x2,y2)
   |x1==x2 = filter (\x-> (fst x == x1)) coordenadas3X3
   |y1==y2 = filter (\y-> (snd y == y1)) coordenadas3X3
filaOColumnaEnComun _ _ = []

casillasEnDireccion :: Tablero -> Int -> Int -> Int -> ([Casilla],[Int])
--le decis tablero, desde, hacia, cuantas
--queremos que el take sea desde la posicon siguiente del desde hasta el final
casillasEnDireccion tablero desde hacia cuantas = (casillasResultado,posicionesResultado)
   where
      desdeEnCoordenadas = intA3x3 desde
      haciaEnCoordenadas = intA3x3 hacia
      fOCEnComun = filaOColumnaEnComun desdeEnCoordenadas haciaEnCoordenadas
      coordenadaDesde = fromMaybe (-1) (elemIndex desdeEnCoordenadas fOCEnComun)
      coordenadasAPartir = subLista fOCEnComun (coordenadaDesde + 1) (length fOCEnComun)
      coordenadasResultado = take cuantas coordenadasAPartir
      posicionesResultado = map tx3Aint coordenadasResultado
      casillasResultado = [tablero!!x | x<-posicionesResultado]

setListaN :: (Eq a) => [a] -> [Int] -> [a] -> [a]
--lista original, lista posiciones, lista de nuevos elementos -> nueva lista
--(posicion,elemento)
setListaN  lista [] [] = lista
setListaN  lista (x:xs) (y:ys) = setListaN (setLista lista x y) xs ys


subLista :: (Eq a) => [a] -> Int -> Int -> [a]
subLista lista desde hasta
      |desde == hasta = [lista!!hasta]
      |desde > hasta = error "indices invalidos"
      |otherwise = take (hasta-desde+1) (drop (desde) lista)

apilarFichasEnCasillas :: [Casilla] -> [Ficha] -> [Int] -> [Casilla]
--casillas, fichas, cuantas apilo en cada casilla
--colocarFichas :: Casilla -> [Ficha] -> Casilla
apilarFichasEnCasillas [] [] [] = []
apilarFichasEnCasillas (casilla1:restoCasillas) fichas (cuantas1:restoCuantas) = [nuevaCasilla1]++(apilarFichasEnCasillas restoCasillas fichasRestantes restoCuantas)
   where
      fichasAColocarEnCasilla1 = take cuantas1 fichas
      fichasRestantes = drop cuantas1 fichas
      nuevaCasilla1 = (colocarFichas casilla1 fichasAColocarEnCasilla1)

mover :: Tablero -> (TakPlayer, TakAction) -> Tablero
mover tablero (jugador,(Mover desde hacia lista)) = nuevoEstadoTablero
   where
      casillaOrigen=tablero!!desde
      cuantasVaAMover = foldr1 (+) lista
      (desapiladasOrigen,nuevoEstadoCasillaOrigen) = desapilarDeCasilla casillaOrigen cuantasVaAMover
      cuantoMeMuevo = length (filter (\x->(x /= 0)) lista)
      (lasOtrasCasillas,posicionesOtrasCasillas) = casillasEnDireccion tablero desde hacia cuantoMeMuevo
      nuevoEstadoDeLasOtrasCasillas = apilarFichasEnCasillas lasOtrasCasillas desapiladasOrigen lista
      nuevoEstadoTablero = setListaN tablero (desde:posicionesOtrasCasillas) (nuevoEstadoCasillaOrigen:nuevoEstadoDeLasOtrasCasillas)
      
      --de la casilla origen tengo que desapilar las primeras (cuantasquieremover - cuantas quiere dejar en el origen) fichas
      -- (0,0) (0,2) [1,1] --en este caso hay que desapilar 2 fichas del origen
      -- (0,0) (0,2) [1,0]
      -- cuntas quiere dejar en segunda posicion es cuantasQuiereMover - cuantas quiera dejar en las otras posiciones

colocarFichas :: Casilla -> [Ficha] -> Casilla
colocarFichas (ConstructorCasilla fichas) fichasNuevas = (ConstructorCasilla (fichas++fichasNuevas))

colocar :: Tablero -> (TakPlayer, TakAction) -> Tablero
colocar tablero (WhitePlayer, (Colocar posicion fichaBlanca)) = nuevoTablero
      where
            nuevoTablero = setLista tablero (posicion) nuevaCasilla
            laCasilla = tablero!!posicion
            nuevaCasilla = colocarFichas laCasilla [fichaBlanca]
colocar tablero (BlackPlayer, (Colocar posicion fichaNegra)) = nuevoTablero
      where
            nuevoTablero = setLista tablero (posicion) nuevaCasilla
            laCasilla = tablero!!posicion
            nuevaCasilla = colocarFichas laCasilla [fichaNegra]
colocar _ _ = error "no implementado"

tableroLleno :: Tablero -> Bool
tableroLleno tablero = length (filter casillaVacia tablero) == 0

fichaDeArriba :: Casilla -> Maybe Ficha
fichaDeArriba (ConstructorCasilla fichas) = if fichas == [] then Nothing else (Just (last fichas))


getJugadorEnFicha :: Maybe Ficha -> Maybe TakPlayer
getJugadorEnFicha Nothing = Nothing
getJugadorEnFicha (Just (Horizontal jug)) = (Just jug)
getJugadorEnFicha (Just (Vertical jug)) = (Just jug)


casillaDeJugador :: TakPlayer -> Casilla -> Bool
casillaDeJugador jugador casilla
   |juga==(Just jugador) = True
   |True = False
        where
            juga = getJugadorEnFicha (fichaDeArriba casilla)
        
posicionesLlenas :: Tablero -> [Int] -> Bool
posicionesLlenas tablero posiciones = foldr1 (&&) llenas
        where
            llenas = [ not (casillaVacia (tablero!!x)) | x <- posiciones]

caminoCompleto :: Tablero -> TakPlayer -> Bool
caminoCompleto tablero jugador

      --si existiera una funcion magica que tire cada una de estas listas se puede
      --hacer un map o algo asi con todas
      -- funcionMagica :: Tablero -> [[Int]]
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
      Nothing -> return $ scoreYResult g
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
      putStrLn (show (length moves))
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