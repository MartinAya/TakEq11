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
import Data.Char
import System.Random --para que cargue bien usar stack ghci
import Data.List.Split
import Control.Monad


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
data TakAction = Colocar Int Ficha | Mover Int Int [Int] | Invalido deriving (Eq, Show)
data Casilla = ConstructorCasilla [Ficha] deriving (Eq,Show)

type Coord = (Int, Int)
type Path = [Coord]

instance Show TakPlayer where
   show WhitePlayer = "W"
   show BlackPlayer = "B"

{-- manejo de listas-}

setLista :: (Eq a) => [a] -> Int -> a -> [a]
setLista lista posicion ele
   |posicion < 0 || posicion > (length lista - 1)= error "indice invalido"
   |True = [if x==posicion then ele else lista!!x | x <- [0..(length lista-1)] ]

--es un set de lista pero se le pasa una lista de elementos, y una lista de posiciones a settear con esos elementos
setListaN :: (Eq a) => [a] -> [Int] -> [a] -> [a]
--lista original, lista posiciones, lista de nuevos elementos -> nueva lista
--(posicion,elemento)
setListaN  [] _ _ = []
setListaN  lista [] _ = lista
setListaN  lista _ [] = lista
setListaN  lista (x:xs) (y:ys) = setListaN (setLista lista x y) xs ys

{-- INICIALIZACION DEL JUEGO Y LOGICA NXN-}

beginning3x3 :: TakGame -- El estado inicial del juego Tak con un tablero de 3x3, con el tablero vacío. 
beginning3x3 = ConstructorTakGame [ ConstructorCasilla [] | _ <- [1..9]] WhitePlayer

beginning4x4 :: TakGame -- El estado inicial del juego Tak con un tablero de 4x4, con el tablero vacío. 
beginning4x4 = ConstructorTakGame [ ConstructorCasilla [] | _ <- [1..16]] WhitePlayer

coordenadasNxN :: Int -> [(Int,Int)]
coordenadasNxN largoTablero = stringIndices
   where
      largoFila = (floor (sqrt (fromIntegral largoTablero)))
      stringIndices = map (\n -> divMod n largoFila ) [0..(largoTablero-1)]

--esta funcion es una "sobrecarga" de coordenadasNxN, para que se pueda llamar con un tablero
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

getTablero :: TakGame -> Tablero
getTablero (ConstructorTakGame tablero _) = tablero

{-- CONTROL DE TURNO : activePlayer, next -}

activePlayer :: TakGame -> Maybe TakPlayer
--activePlayer tiene que verificar si alguien completo un camino, y si es así, devolver Nothing

activePlayer g@(ConstructorTakGame tablero _)
   |(caminoCompleto  (ConstructorTakGame tablero WhitePlayer)) || (caminoCompleto (ConstructorTakGame tablero BlackPlayer)) = Nothing
   |sinFichas = Nothing
   |otherwise = listToMaybe [p | (p, as) <- actions g, not (null as)]
   where
      whitesinFichas = (fichasDeJugador tablero WhitePlayer)==(nFichasXJugador tablero)
      blacksinFichas = (fichasDeJugador tablero BlackPlayer)==(nFichasXJugador tablero)
      sinFichas = whitesinFichas || blacksinFichas

next :: TakGame -> (TakPlayer, TakAction) -> TakGame -- Esta función aplica una acción sobre un estado de juego dado, y retorna 
                                                         -- jugador activo, si el juego está terminado, o si la acción no es realizable. 
--colocar :: Tablero -> (TakPlayer, TakAction) -> Tablero
next (ConstructorTakGame casillas WhitePlayer) (jugador, c@(Colocar cas ficha) ) = ConstructorTakGame (colocar casillas (jugador, c ) ) (BlackPlayer)    
next (ConstructorTakGame casillas BlackPlayer) (jugador, c@(Colocar cas ficha) ) = ConstructorTakGame (colocar casillas (jugador, c ) ) (WhitePlayer)                                                
--mover :: Tablero -> (TakPlayer, TakAction) -> Tablero
next (ConstructorTakGame casillas WhitePlayer) (jugador, m@(Mover desde hasta apila) ) = ConstructorTakGame (mover casillas (jugador, m ) ) (BlackPlayer)    
next (ConstructorTakGame casillas BlackPlayer) (jugador, m@(Mover desde hasta apila) ) = ConstructorTakGame (mover casillas (jugador, m ) ) (WhitePlayer)
--invalido
next (ConstructorTakGame casillas WhitePlayer) (jugador, invalido) = ConstructorTakGame (casillas) (WhitePlayer)
next (ConstructorTakGame casillas BlackPlayer) (jugador, invalido) = ConstructorTakGame (casillas) (BlackPlayer)

{-- LOGICA de movimientos: actions, colocar, mover -}

actions :: TakGame -> [(TakPlayer, [TakAction])]
actions g@(ConstructorTakGame tablero activo)
   |totalFichas == 0 = [(BlackPlayer,[]),(WhitePlayer, posiblesColocarHBlack)]--primer turno blancas : coloca una negra
   |totalFichas == 1 = [(WhitePlayer,[]),(BlackPlayer,posiblesColocarHWhite)]
   |activo == WhitePlayer = [(BlackPlayer,[]),(WhitePlayer,posiblesColocarHWhite++posiblesColocarVWhite++posiblesMover2)]
   |activo == BlackPlayer  = [(WhitePlayer,[]),(BlackPlayer,posiblesColocarHBlack++posiblesColocarVBlack++posiblesMover2)]
      where
         posiblesColocarHWhite = [Colocar cas (Horizontal WhitePlayer) | cas <- posicionesVacias ] 
         posiblesColocarVWhite = [Colocar cas (Vertical WhitePlayer) | cas <- posicionesVacias ]
         posiblesColocarHBlack = [Colocar cas (Horizontal BlackPlayer) | cas <- posicionesVacias ] 
         posiblesColocarVBlack = [Colocar cas (Vertical BlackPlayer) | cas <- posicionesVacias ]

         posicionesVacias = [ x | x <- [0..(length tablero - 1)], casillaVacia (tablero!!x)]
         listaDesdes = posicionesDeJugador g
         desdesYHastas = foldr (++) [] (map (calculadorHacia g) listaDesdes)
         posiblesMover1 = [ (posiblesMoverEnTupla desde hasta tablero) | (desde,hasta) <- desdesYHastas]
         posiblesMover2 = foldr (++) [] posiblesMover1
         totalFichas = (fichasDeJugador tablero WhitePlayer)+(fichasDeJugador tablero BlackPlayer)
actions _ = error "actions: error"

-- define la accion de mover una ficha, devuelve nuevo estado de tablero
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

-- apila fichas, en la pila de fichas de una casilla
colocarFichas :: Casilla -> [Ficha] -> Casilla
colocarFichas (ConstructorCasilla fichas) fichasNuevas = (ConstructorCasilla (fichas++fichasNuevas))

-- define la accion de colocar una ficha, devuelve nuevo estado de tablero
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

orientacionFicha:: Ficha -> String
orientacionFicha (Horizontal _) = "H"
orientacionFicha (Vertical _)= "V"

--esta funcion devuelve todos los posibles TakAction Mover, entre dos posiciones del tablero
posiblesMoverEnTupla :: Int -> Int -> Tablero -> [TakAction]
posiblesMoverEnTupla desde hacia tablero = posiblesMover
   where 
      hastaCuantasFichas = take fichasActuales (cuantasFichasPuedeMover tablero)
      (ConstructorCasilla fichas) = (tablero!!desde)
      fichasActuales = length fichas
      apilamientos = foldr1 (++) [ posiblesApilamientos tablero cuantas desde hacia | cuantas <- hastaCuantasFichas ]
      posiblesMover = [ Mover desde hacia a | a <- apilamientos]

--esta funcion recibe una pila de fichas, y las distribuye en las casillas tambien dadas por parametro
apilarFichasEnCasillas :: [Casilla] -> [Ficha] -> [Int] -> [Casilla]
--casillas, fichas, cuantas apilo en cada casilla
apilarFichasEnCasillas _ _ [] = []
apilarFichasEnCasillas _ [] _ = []
apilarFichasEnCasillas [] _ _ = []
apilarFichasEnCasillas (casilla1:restoCasillas) fichas (cuantas1:restoCuantas) = [nuevaCasilla1]++(apilarFichasEnCasillas restoCasillas fichasRestantes restoCuantas)
   where
      fichasAColocarEnCasilla1 = take cuantas1 fichas
      fichasRestantes = drop cuantas1 fichas
      nuevaCasilla1 = (colocarFichas casilla1 fichasAColocarEnCasilla1)

cuantasFichasPuedeMover :: Tablero -> [Int]
cuantasFichasPuedeMover tablero
   |length tablero == 9 = [1,2,3]
   |length tablero == 16 = [1,2,3,4]

--solo devuelve distancia para casillas en misma fila o columna
distancia :: (Int,Int) -> (Int,Int) -> Int
distancia (x1,y1) (x2,y2) 
   |x1==x2 = abs (y2 - y1)
   |y1==y2 = abs (x2 - x1)
   |otherwise = error "No hay filas ni columnas en comun"
   

--devuelve las fichas desapiladas en una lista, y el nuevo estado de la casilla
desapilarDeCasilla :: Casilla -> Int -> ([Ficha],Casilla)
desapilarDeCasilla (ConstructorCasilla fichas) cuantas = (desapiladas,casillaNueva)
      where
         desapiladas = drop ((length fichas) - cuantas) fichas
         nuevaPilaEnCasilla = take ((length fichas) - cuantas) fichas
         casillaNueva = (ConstructorCasilla nuevaPilaEnCasilla)


filaOColumnaEnComun :: Tablero -> (Int,Int) -> (Int,Int) -> [(Int,Int)]
filaOColumnaEnComun tablero (x1,y1) (x2,y2)
   |x1==x2 && y1<y2 = filter (\x-> (fst x == x1) && (snd x <= y2) && (snd x > y1 ) ) (coordenadasNxNT tablero)  --de izquierda a derecha
   |x1==x2 && y1>y2 = reverse (filter (\x-> (fst x == x1) && (snd x >= y2) && (snd x < y1)) (coordenadasNxNT tablero))  --de derecha a izquierda

   |y1==y2 && x1>x2 = reverse (filter (\y-> (snd y == y1) && (fst y >= x2) && (fst y < x1)) (coordenadasNxNT tablero))  --de arriba para abajo   -- invertir esta lista
   |y1==y2 && x1<x2 = filter (\y-> (snd y == y1) && (fst y <= x2) && (fst y > x1)) (coordenadasNxNT tablero )  --de abajo para arriba
filaOColumnaEnComun _ _ _= []

--devuelve las casillas que estan a partir de una casilla de origen, en direccion a otra, y determinada dsitancia
casillasEnDireccion :: Tablero -> Int -> Int -> Int -> ([Casilla],[Int])
casillasEnDireccion tablero desde hacia cuantas = (casillasResultado,posicionesResultado)
   where
      desdeEnCoordenadas = intANxN tablero desde
      haciaEnCoordenadas = intANxN tablero hacia
      fOCEnComun = filaOColumnaEnComun tablero desdeEnCoordenadas haciaEnCoordenadas
      posicionesResultado = take cuantas (map (nxNAint tablero) fOCEnComun)
      casillasResultado = [tablero!!x | x<-posicionesResultado]

--devuelve posibles apilamientos con determinadas fichas, y posiciones finales e iniciales
posiblesApilamientos :: Tablero -> Int -> Int -> Int -> [[Int]]
posiblesApilamientos tablero cuantas desde hasta = apilamientosValidos
   where
      espacioEntreDesdeYHasta = distancia (intANxN tablero desde) (intANxN tablero hasta)
      todosPosiblesApilamientos = replicateM espacioEntreDesdeYHasta [0..cuantas]
      apilamientosValidos = filter (filtrado cuantas) todosPosiblesApilamientos
      
--controla que el apilamiento posible, tenga la cantidad inidicada de fichas    
filtrado :: Int -> [Int] -> Bool
filtrado cuantas lista = (sinCerosALaIzquierda lista) && (foldr1 (+) lista == cuantas)
--1 - la suma tiene que ser cuantas

--Controla que no halla un cero a la izquierda, porque en un apilamiento no se puede saltearse una casilla
sinCerosALaIzquierda :: [Int] -> Bool
sinCerosALaIzquierda [x] = True
sinCerosALaIzquierda (x:y:xs)
   |x==0 && y/=0 = False
   |True = sinCerosALaIzquierda (y:xs)

--devuelve a a partir de un origen, todas las direcciones en las que se puede ir
calculadorHacia :: TakGame -> Int -> [(Int,Int)]
calculadorHacia g desde = resultado
   where
      hacias = direccionesPosibles g desde
      resultado = [(desde,x) | x <- hacias]

posicionesDeJugador :: TakGame -> [Int]
posicionesDeJugador (ConstructorTakGame tablero activo) = indicesPosiciones
      where
         indicesPosiciones = findIndices (casillaDeJugador activo) tablero

--Esta funcion se encarga de obtener el elemento más lejano en una direccion dada
masLejano :: Tablero -> Int -> [Int] -> Int
masLejano tablero origen posibles = elMasLejano
   where
      origenNxN = intANxN tablero  origen
      posiblesNxN = map (intANxN tablero) posibles
      distancias = map (distancia origenNxN) posiblesNxN
      mayorDistancia = maximum distancias
      posicionDelMayor = fromMaybe 0 (elemIndex mayorDistancia distancias)
      elMasLejano = posibles!!posicionDelMayor

-- Esta funcion se encarga de obtener a partir de un juego y una posicion de casilla todas 
-- las direcciones en las que se puede realizar un movimiento
direccionesPosibles :: TakGame -> Int -> [Int]   
direccionesPosibles g@(ConstructorTakGame tablero _) casilla = derecha2++izquierda2++arriba2++abajo2
   where
      (x0,y0) = intANxN tablero casilla
      derecha =  map (nxNAint tablero)  (filter (\(x,y) -> x == x0 && y > y0) (coordenadasNxNT tablero)) 
      izquierda =  map (nxNAint tablero) (filter (\(x,y) -> x == x0 && y < y0) (coordenadasNxNT tablero))
      arriba =  map (nxNAint tablero) (filter (\(x,y) -> x > x0 && y == y0) (coordenadasNxNT tablero))
      abajo = map (nxNAint tablero) (filter (\(x,y) -> x < x0 && y == y0) (coordenadasNxNT tablero))

      derechaSinV = filter (verticalEntre g casilla) derecha
      izquierdaSinV = filter (verticalEntre g casilla) izquierda
      arribaSinV = filter (verticalEntre g casilla) arriba
      abajoSinV = filter (verticalEntre g casilla) abajo
      
      derecha2 = if derechaSinV /= [] then [(masLejano tablero casilla derechaSinV)] else []
      izquierda2 = if izquierdaSinV /= [] then [(masLejano tablero casilla izquierdaSinV)] else []
      arriba2 = if arribaSinV /= [] then [(masLejano tablero casilla arribaSinV)] else []
      abajo2 = if abajoSinV /= [] then [(masLejano tablero casilla abajoSinV)] else []

--verifica si hay una ficha vertical entre las posiciones pasadas por parametro
verticalEntre :: TakGame -> Int -> Int -> Bool
verticalEntre (ConstructorTakGame tablero _) a b = algunaVertical 
   where
      recorridoEntreAyB = map (nxNAint tablero) (filaOColumnaEnComun tablero (intANxN tablero a) (intANxN tablero b))
      casillas = map (tablero!!) recorridoEntreAyB
      algunaVertical = foldr (&&) True  (map casillaSinVerticalArriba casillas)

casillaSinVerticalArriba :: Casilla -> Bool
casillaSinVerticalArriba casilla = "H" == orientacion
   where
      ficha = fromMaybe (Horizontal WhitePlayer) (fichaDeArriba casilla)
      orientacion = orientacionFicha ficha

casillaVacia :: Casilla -> Bool
casillaVacia (ConstructorCasilla [] ) = True
casillaVacia _ = False

jugadorEnFicha :: TakPlayer -> Ficha -> Bool
jugadorEnFicha jug (Horizontal jugador) = jug == jugador 
jugadorEnFicha jug (Vertical jugador) = jug == jugador

{-- LOGICA de resultado: score, result -}

result :: TakGame -> [(TakPlayer, Int)]
-- 1 es que ganó, (-1) perdió, 0 empató
-- se asume que cuando se llama a result, el juego ya terminó
-- result evalua el estado actual para decir quien gano, o tirar un score
result g@(ConstructorTakGame tablero jugadorActual)
   |caminoCompletoWhite = [(WhitePlayer,1),(BlackPlayer,(-1))]
   |caminoCompletoBlack = [(WhitePlayer,(-1)),(BlackPlayer,(1))]
   |tablerollen = [(WhitePlayer,0),(BlackPlayer,(0))]
   |blacksinFichas = [(WhitePlayer,1),(BlackPlayer,(-1))]
   |whitesinFichas = [(WhitePlayer,(-1)),(BlackPlayer,(1))]
      where
         caminoCompletoWhite = caminoCompleto (ConstructorTakGame tablero WhitePlayer)
         caminoCompletoBlack = caminoCompleto (ConstructorTakGame tablero BlackPlayer)
         tablerollen = tableroLleno tablero
         whitesinFichas = (fichasDeJugador tablero WhitePlayer)==(nFichasXJugador tablero)
         blacksinFichas = (fichasDeJugador tablero BlackPlayer)==(nFichasXJugador tablero)

score :: TakGame -> [(TakPlayer, Int)]
score g = zip players [scoreBlancas, scoreNegras]
   where
      scoreBlancas = casillasHJugador g WhitePlayer
      scoreNegras = casillasHJugador g BlackPlayer
    
nFichasXJugador :: Tablero -> Int
nFichasXJugador tablero
   |casillas == 9 = 10
   |casillas == 16 = 15
   |casillas == 25 = 21
   |casillas == 36 = 30
   where
      casillas = length tablero

fichasDeJugadorCasilla :: TakPlayer -> Casilla -> Int
fichasDeJugadorCasilla jugador (ConstructorCasilla fichas) = length (filter (jugadorEnFicha jugador) fichas)

fichasDeJugador :: Tablero -> TakPlayer -> Int
fichasDeJugador tablero jugador = foldr1 (+) (map (fichasDeJugadorCasilla jugador) tablero)
      
casillasHJugador :: TakGame -> TakPlayer -> Int
casillasHJugador g@(ConstructorTakGame tablero _) p = length (filter (==True) (map (casillaGanadora (ConstructorTakGame tablero p)) coords))
   where
      coords = coordenadasNxNT (getTablero g)

ganador :: [(TakPlayer, Int)] -> Maybe TakPlayer
ganador [(WhitePlayer,1),(BlackPlayer,(-1))] = Just WhitePlayer
ganador [(WhitePlayer,(-1)),(BlackPlayer,1)] = Just BlackPlayer
ganador [(WhitePlayer,(0)),(BlackPlayer,0)] = Nothing

scoreYResult :: TakGame -> [(TakPlayer, Int)]
scoreYResult game@(ConstructorTakGame tablero _) = resultado++puntaje
   where 
      resultado = result game 
      puntaje= score game 

caminoCompleto :: TakGame -> Bool
caminoCompleto g@(ConstructorTakGame tablero jugador) = (length caminosGanadores) /= 0

      where
         largoTablero = length tablero
         largoFila = (floor (sqrt (fromIntegral largoTablero)))
         iniciales = inicialesGanadoras g                
         pathsInciales = [ [[x]] | x <- iniciales]       
         caminos = (map (walks g largoFila) pathsInciales) 
         caminos2 = foldr (++) [] caminos          
         caminosGanadores = filter (caminoGanador tablero) caminos2

{- VALIDACION CAMINO GANADOR-}

inicialesGanadoras :: TakGame -> [Coord]
inicialesGanadoras g = primeraFilaYColumnaFiltrada
   where
      primeraFilaYColumna = primeraFyC g
      primeraFilaYColumnaFiltrada = filter (casillaGanadora g) primeraFilaYColumna

primeraFyC :: TakGame -> [Coord]
primeraFyC (ConstructorTakGame tablero _) = [ (x,y) | (x,y) <- coordenadasn , x==0 || y ==0]
   where
      coordenadasn = coordenadasNxNT tablero

orthDeltas :: [Coord]
orthDeltas = [(x, y) | x <- [-1..1], y <- [-1..1], (x == 0) /= (y == 0)]

insideBoard :: Int -> Coord-> Bool
insideBoard size (x, y) = x >= 0 && y >= 0 && x < size && y < size

orthAdjacent :: TakGame -> Int -> Coord -> [Coord] 
orthAdjacent g size (x, y) = filter ganadora (filter adentro lista)
   where 
      adentro = insideBoard size
      ganadora = casillaGanadora g  
      lista = [(x + dx, y + dy) | (dx, dy) <- orthDeltas]

walks :: TakGame ->  Int -> [Path] -> [Path]
walks g size paths
  -- Punto fijo, la lista de entrada y resultado tiene los mismo valores.
  | null (nextPaths \\ paths) = paths
  -- Se cambiaron los caminos, hay que seguir calculando.
  | otherwise = walks g size nextPaths
  where step = [next:path | path <- paths, next <- orthAdjacent g size (head path), notElem next path ]
        nextPaths = filter (\p -> (length p) <= 2 * (size - 1)) step

casillaGanadora :: TakGame -> (Int,Int) -> Bool
casillaGanadora g@(ConstructorTakGame tablero jugador)  posicion  = ok
   where
      pos = nxNAint tablero posicion
      casilla = tablero!!pos
      fichaArriba = fichaDeArriba casilla
      ok = fichaArriba == (Just (Horizontal jugador))

caminoGanador :: Tablero -> Path -> Bool
caminoGanador tablero camino = abs (x1 - x2) == distanciaNecesariaParaGanar || abs (y1 - y2) == distanciaNecesariaParaGanar
   where
      largoTablero = length tablero
      (x1,y1) = head camino
      (x2,y2) = last camino
      distanciaNecesariaParaGanar = (floor (sqrt (fromIntegral largoTablero))) - 1


{- parseo de entradas por terminal, y salidas por terminal  -}

readAction :: Tablero -> String -> TakAction
readAction tablero entrada = readAction2 tablero (splitOn "," entrada)

showBoard :: TakGame -> String -- Convierte el estado de juego a un texto que puede ser impreso en la consola para mostrar el tablero y demás información de la partida. 
showBoard (ConstructorTakGame tablero WhitePlayer) = "Le toca a Blancas " ++ "\n" ++  (showTablero tablero)
showBoard (ConstructorTakGame tablero BlackPlayer) = "Le toca a Negras "++ "\n" ++ (showTablero tablero)

showAction :: Tablero -> TakAction -> String
showAction tablero (Colocar int ficha) = "C " ++ show (intANxN tablero int) ++" "++ showFicha (ficha)
showAction tablero (Mover desde direccion apilamiento) = "M " ++ show (intANxN tablero desde) ++" "++ show (intANxN tablero direccion) ++ (show apilamiento)

readCoord :: Tablero -> [String] -> Int
readCoord tablero [x0,y0] = elInt
   where
      elInt = nxNAint tablero (intx0,inty0)
      intx0 = read x0
      inty0 = read y0
readCoord _ _= error "error readCoord"

readAction2 :: Tablero -> [String] -> TakAction
--data TakAction = Colocar Int Ficha | Mover Int Int [Int] deriving (Eq, Show
--data Ficha = Horizontal TakPlayer | Vertical TakPlayer deriving (Eq,Show)
readAction2 tablero ["C",x0,y0,"H","W"] = Colocar (readCoord tablero [x0,y0]) (Horizontal WhitePlayer)
readAction2 tablero ["C",x0,y0,"H","B"] = Colocar (readCoord tablero [x0,y0]) (Horizontal BlackPlayer)
readAction2 tablero ["C",x0,y0,"V","W"] = Colocar (readCoord tablero [x0,y0]) (Vertical WhitePlayer)
readAction2 tablero ["C",x0,y0,"V","B"] = Colocar (readCoord tablero [x0,y0]) (Vertical BlackPlayer)
readAction2 tablero ["M",x0,y0,x1,y1,lista] = Mover (readCoord tablero [x0,y0]) (readCoord tablero [x1,y1]) (map digitToInt lista)
readAction2 _ _ = Invalido

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
      
showFicha :: Ficha -> String
showFicha (Horizontal ply) = " H" ++ "-" ++show (ply) 
showFicha (Vertical ply) = " V" ++ "-" ++show (ply) 

showCasilla :: Casilla -> String
showCasilla (ConstructorCasilla []) = " vacia" ++ "\n"
showCasilla (ConstructorCasilla fichas) = (foldr1 (++) (map showFicha fichas)) ++ "\n"
    
showTablero :: Tablero -> String
showTablero tablero = foldr1 (++) (indiceYCasilla)
        where
            largoTablero = length tablero
            largoFila = (floor (sqrt (fromIntegral largoTablero)))
            stringCasillas = map showCasilla tablero
            stringIndices = map (\n -> divMod n largoFila ) [0..(largoTablero-1)]
            indiceYCasilla = zipWith (++) (map show stringIndices) stringCasillas

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
      Nothing -> return $ scoreYResult g --se cambio score por scoreYResult
      Just p -> do
         let ag = [ag1, ag2] !! (fromJust (elemIndex p players))
         move <- ag g
         runMatch ags (Tak.next g (p, fromJust move))

{- La función ´runOnConsole´ ejecuta toda la partida a partir del estado inicial usando dos agentes
de consola.
-}
--en la llamada a runMatch WhitePlayer siempre debe ser el primer argumento, y BlackPlayer el segundo
runOnConsole :: TakGame -> IO [(TakPlayer, Int)]
runOnConsole g = do
   runMatch (consoleAgent WhitePlayer, consoleAgent BlackPlayer) g

run3x3OnConsole :: IO [(TakPlayer, Int)]
run3x3OnConsole = runOnConsole beginning3x3

run4x4OnConsole :: IO [(TakPlayer, Int)]
run4x4OnConsole = runOnConsole beginning4x4

runRandom3x3 :: IO [(TakPlayer, Int)]
runRandom3x3 = do
   runMatch (randomAgent WhitePlayer, randomAgent BlackPlayer) beginning3x3

runRandom4x4 :: IO [(TakPlayer, Int)]
runRandom4x4 = do
   runMatch (randomAgent WhitePlayer, randomAgent BlackPlayer) beginning4x4

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
      putStrLn ("Select one move:" ++ concat ["\n"++ showAction (getTablero state) m | m <- moves])--se cambio show por showAction
      putStrLn (show (length moves) ++ "movimientos")
      line <- getLine
      let input = readAction (getTablero state) line --se cambio readAction para que reciba el tablero
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