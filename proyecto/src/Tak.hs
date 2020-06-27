module Tak
()
where

    data TakPlayer = WhitePlayer | BlackPlayer deriving (Eq, Show, Enum)
    --unir los tipos algebraicos orientacion + ficha
    data Orientacion = Horizontal | Vertical deriving (Eq, Show, Enum)
    
    data Ficha = ConstructorFicha Orientacion TakPlayer deriving (Eq,Show)
    
    data Casilla = ConstructorCasilla [Ficha] Int deriving (Eq,Show)
                                            -- (0,0)
    type Tablero = [Casilla]
    --en vez de booleano, usar takPlayer
    data TakGame = ConstructorTakGame Tablero Bool deriving (Eq,Show)
    --hacer Mover Int Int Ficha
    data TakAction = Colocar Int Ficha | Mover Casilla Casilla Ficha deriving (Eq,Show)

    beginning3x3 :: TakGame -- El estado inicial del juego Tak con un tablero de 3x3, con el tablero vacío. 
    beginning3x3 = ConstructorTakGame [ ConstructorCasilla [] x | x <- [1..9]] True
    
    beginning4x4 :: TakGame -- El estado inicial del juego Tak con un tablero de 4x4, con el tablero vacío. 
    beginning4x4 = ConstructorTakGame [ ConstructorCasilla [] x | x <- [1..16]] True
    
    activePlayer :: TakGame -> TakPlayer -- Esta función determina a cuál jugador le toca mover, dado un estado de juego.
    activePlayer (ConstructorTakGame _ True) = WhitePlayer
    activePlayer (ConstructorTakGame _ False) = BlackPlayer

    casillaVacia :: Casilla -> Bool
    casillaVacia (ConstructorCasilla [] _) = True
    otherwise = False

    casillasVacias :: TakGame -> Tablero
    casillasVacias (ConstructorTakGame lista _) = filter casillaVacia lista

    getPosicionCasilla :: Casilla -> Int
    getPosicionCasilla (ConstructorCasilla _ ind) = ind
    
    actions :: TakGame -> [(TakPlayer, [TakAction])] -- La lista debe incluir una y solo una tupla para cada jugador. 
                                                     -- Si el jugador está activo, la lista asociada debe incluir todos sus 
                                                     -- posibles movimientos para el estado de juego dado. Sino la lista debe estar vacía.
    actions juego                                    
        |activo==WhitePlayer = [(BlackPlayer,[]) , (WhitePlayer, [Colocar cas fichaBlanca | cas <- posicionesVacias ]  ) ]
        |activo==BlackPlayer = [(BlackPlayer, [Colocar cas fichaNegra | cas <- posicionesVacias]  ) , (WhitePlayer,[]) ]
        where
                activo = activePlayer juego 
                vacias = casillasVacias juego                                      
                fichaBlanca = ConstructorFicha Horizontal WhitePlayer
                fichaNegra = ConstructorFicha Horizontal BlackPlayer
                posicionesVacias = map getPosicionCasilla vacias
    actions _ = error "no existis"        
    
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
    apilarFicha (ConstructorCasilla fichas ind) fichaNueva = (ConstructorCasilla (fichaNueva:(fichas)) ind)

    next :: TakGame -> (TakPlayer, TakAction) -> TakGame -- Esta función aplica una acción sobre un estado de juego dado, y retorna 
                                                         -- jugador activo, si el juego está terminado, o si la acción no es realizable. 
    next (ConstructorTakGame casillas turno) (jugador, (Colocar cas ficha) ) = ConstructorTakGame (colocar casillas (jugador, (Colocar cas ficha) ) ) (not turno)                                                    -- el estado resultante. Se debe levantar un error si el jugador dado no es el 
    next _ _ = error "no implementado"                                                
    

    colocar :: Tablero -> (TakPlayer, TakAction) -> Tablero
    colocar tablero (WhitePlayer, (Colocar posicion fichaBlanca)) = nuevoTablero
        where
            nuevoTablero = setLista tablero (posicion-1) nuevaCasilla
            laCasilla = tablero!!posicion
            nuevaCasilla = apilarFicha laCasilla (ConstructorFicha Horizontal WhitePlayer)
    colocar tablero (BlackPlayer, (Colocar posicion fichaNegra)) = nuevoTablero
        where
            nuevoTablero = setLista tablero (posicion-1) nuevaCasilla
            laCasilla = tablero!!posicion
            nuevaCasilla = apilarFicha laCasilla (ConstructorFicha Horizontal BlackPlayer)
    colocar _ _ = error "no implementado"

    tableroLleno :: Tablero -> Bool
    tableroLleno tablero = length (filter casillaVacia tablero) == 0

    fichaDeArriba :: Casilla -> Ficha
    fichaDeArriba (ConstructorCasilla fichas num) = last fichas

    casillaDeJugador :: TakPlayer -> Casilla -> Bool
    casillaDeJugador jugador casilla = jugador == jug
        where
            (ConstructorFicha ori jug) = fichaDeArriba casilla
        
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
            tableroSoloConFichasJugador = map (vaciarCasillaSi jugador) tablero
    
    getIndiceCasilla :: Casilla -> Int
    getIndiceCasilla (ConstructorCasilla _ ind) = ind
    
    vaciarCasillaSi :: TakPlayer -> Casilla -> Casilla
    vaciarCasillaSi jugador casilla
        |jugador==jug = casilla
        |True = ConstructorCasilla [] (getIndiceCasilla casilla)
            where
                (ConstructorFicha ori jug) = fichaDeArriba casilla

    showFicha :: Ficha -> String
    showFicha (ConstructorFicha ori ply) = show (ori) ++ "-" ++show (ply) ++ ","


--putStr (showGame beginning3x3)
    showCasilla :: Casilla -> String
    showCasilla (ConstructorCasilla [] ind) = show(ind) ++ " casilla vacia" ++ "\n"
    showCasilla (ConstructorCasilla fichas ind) = (show(ind) ++" "++ (foldr1 (++) (map showFicha fichas))) ++ "\n"
    

    showTablero :: Tablero -> String
    showTablero tablero = foldr1 (++) (map showCasilla tablero)  
    -- data Ficha = ConstructorFicha Orientacion TakPlayer deriving (Eq,Show)
    --data TakGame = ConstructorTakGame Tablero Bool deriving (Eq,Show)
    showGame :: TakGame -> String -- Convierte el estado de juego a un texto que puede ser impreso en la consola para mostrar el tablero y demás información de la partida. 
    showGame (ConstructorTakGame tablero True) = "Le toca a Blancas " ++ "\n" ++  (showTablero tablero)
    showGame (ConstructorTakGame tablero False) = "Le toca a Negras "++ "\n" ++ (showTablero tablero)
    
    printGame :: TakGame -> IO ()
    printGame juego = putStr (showGame juego)


    --next (beginning3x3) (WhitePlayer, (Colocar 1 (ConstructorFicha Horizontal WhitePlayer)  )
    --





    -- result :: TakGame -> [(TakPlayer, Int)] -- Si el juego está terminado retorna el resultado de juego para cada jugador. Este valor es 1 si el jugador ganó, -1 si perdió y 0 si se empató. Si el juego no está terminado, se debe retornar una lista vacía. 
    -- score :: TakGame -> [(TakPlayer, Int)] -- Retorna el puntaje para todos los jugadores en el estado de juego dado. Esto es independiente de si el juego está terminado o no. 
    
    -- showAction :: TakAction -> String -- Convierte una acción a un texto que puede ser impreso en la consola para mostrarla. 
    -- readAction :: String -> TakAction -- Obtiene una acción a partir de un texto que puede haber sido introducido por el usuario en la consola. 