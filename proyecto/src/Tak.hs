module Tak
()
where

    data TakPlayer = WhitePlayer | BlackPlayer deriving (Eq, Show, Enum)
    
    data Ficha = Horizontal TakPlayer | Vertical TakPlayer deriving (Eq,Show)
    
    data Casilla = ConstructorCasilla [Ficha] deriving (Eq,Show)
                                            -- (0,0)
    type Tablero = [Casilla]

    data TakGame = ConstructorTakGame Tablero TakPlayer deriving (Eq,Show)
    --hacer Mover Int Int Ficha
    data TakAction = Colocar Int Ficha | Mover Int Int Ficha deriving (Eq,Show)

    beginning3x3 :: TakGame -- El estado inicial del juego Tak con un tablero de 3x3, con el tablero vacío. 
    beginning3x3 = ConstructorTakGame [ ConstructorCasilla [] | x <- [1..9]] WhitePlayer
    
    beginning4x4 :: TakGame -- El estado inicial del juego Tak con un tablero de 4x4, con el tablero vacío. 
    beginning4x4 = ConstructorTakGame [ ConstructorCasilla [] | x <- [1..16]] WhitePlayer
    
    activePlayer :: TakGame -> TakPlayer -- Esta función determina a cuál jugador le toca mover, dado un estado de juego.
    activePlayer (ConstructorTakGame _ WhitePlayer) = WhitePlayer
    activePlayer (ConstructorTakGame _ BlackPlayer) = BlackPlayer

    casillaVacia :: Casilla -> Bool
    casillaVacia (ConstructorCasilla [] ) = True
    otherwise = False
    
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
    
    getTablero :: TakGame -> Tablero
    getTablero (ConstructorTakGame tablero _) = tablero

    -- setLista :: (Eq a) => [a] -> Int -> a -> [a]
    -- setLista lista 0 ele = ele : subLista lista (1) (length lista)
    -- setLista lista lugar ele = subLista lista 0 (lugar-1) ++ [ele] ++  subLista lista (lugar+1) (length lista)
    
    -- subLista :: (Eq a) => [a] -> Int -> Int -> [a]
    -- subLista lista desde hasta
    --    |desde == hasta = [lista!!hasta]
    --    |desde > hasta = error "indices invalidos"
    --    |True = take (hasta-desde+1) (drop (desde) lista)

    setLista :: (Eq a) => [a] -> Int -> a -> [a]
    setLista lista posicion ele
        |posicion < 0 || posicion > (length lista - 1)= error "indice invalido"
        |True = [if x==posicion then ele else lista!!x | x <- [0..(length lista-1)] ]
    
    apilarFicha :: Casilla -> Ficha -> Casilla
    apilarFicha (ConstructorCasilla fichas) fichaNueva = (ConstructorCasilla (fichaNueva:(fichas)))

    next :: TakGame -> (TakPlayer, TakAction) -> TakGame -- Esta función aplica una acción sobre un estado de juego dado, y retorna 
                                                         -- jugador activo, si el juego está terminado, o si la acción no es realizable. 
    next (ConstructorTakGame casillas WhitePlayer) (jugador, (Colocar cas ficha) ) = ConstructorTakGame (colocar casillas (jugador, (Colocar cas ficha) ) ) (BlackPlayer)                                                    -- el estado resultante. Se debe levantar un error si el jugador dado no es el 
    next _ _ = error "no implementado"                                                
    

    colocar :: Tablero -> (TakPlayer, TakAction) -> Tablero
    colocar tablero (WhitePlayer, (Colocar posicion fichaBlanca)) = nuevoTablero
        where
            nuevoTablero = setLista tablero (posicion-1) nuevaCasilla
            laCasilla = tablero!!posicion
            nuevaCasilla = apilarFicha laCasilla (Horizontal WhitePlayer)
    colocar tablero (BlackPlayer, (Colocar posicion fichaNegra)) = nuevoTablero
        where
            nuevoTablero = setLista tablero (posicion-1) nuevaCasilla
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
    showFicha (Horizontal ply) = "Horizontal" ++ "-" ++show (ply) ++ ","
    showFicha (Vertical ply) = "Vertical" ++ "-" ++show (ply) ++ ","


    --putStr (showGame beginning3x3)
    showCasilla :: Casilla -> String
    showCasilla (ConstructorCasilla []) = " casilla vacia" ++ "\n"
    showCasilla (ConstructorCasilla fichas) = (foldr1 (++) (map showFicha fichas)) ++ "\n"
    

    showTablero :: Tablero -> String
    showTablero tablero = foldr1 (++) (map showCasilla tablero)
        where
            showCasillaConIndices = [ x++" "++ show (y) | x <- showCasillas, y <- [1..(length showCasillas + 1)]]
            showCasillas = map showCasilla tablero


    -- data Ficha = ConstructorFicha Orientacion TakPlayer deriving (Eq,Show)
    --data TakGame = ConstructorTakGame Tablero Bool deriving (Eq,Show)
    showGame :: TakGame -> String -- Convierte el estado de juego a un texto que puede ser impreso en la consola para mostrar el tablero y demás información de la partida. 
    showGame (ConstructorTakGame tablero WhitePlayer) = "Le toca a Blancas " ++ "\n" ++  (showTablero tablero)
    showGame (ConstructorTakGame tablero BlackPlayer) = "Le toca a Negras "++ "\n" ++ (showTablero tablero)
    
    printGame :: TakGame -> IO ()
    printGame juego = putStr (showGame juego)


    --next (beginning3x3) (WhitePlayer, (Colocar 1 (ConstructorFicha Horizontal WhitePlayer)  )
    --





    -- result :: TakGame -> [(TakPlayer, Int)] -- Si el juego está terminado retorna el resultado de juego para cada jugador. Este valor es 1 si el jugador ganó, -1 si perdió y 0 si se empató. Si el juego no está terminado, se debe retornar una lista vacía. 
    -- score :: TakGame -> [(TakPlayer, Int)] -- Retorna el puntaje para todos los jugadores en el estado de juego dado. Esto es independiente de si el juego está terminado o no. 
    
    -- showAction :: TakAction -> String -- Convierte una acción a un texto que puede ser impreso en la consola para mostrarla. 
    -- readAction :: String -> TakAction -- Obtiene una acción a partir de un texto que puede haber sido introducido por el usuario en la consola. 