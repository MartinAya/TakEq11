
module Lib
    ( subLista,
     rosenbrock,
     delta,
     surroundings,
     minRosenbrock,
     posicionMin
    -- localMinimum,
    ) 
    where
        import Data.List
        import Data.Maybe

        --take
        --creates a list, the first argument determines, 
        --how many items should be taken from the list passed as the second argument
        --Input: take 5 [1,2,3,4,5,6,7]
        --Output: [1,2,3,4,5]
        --drop
        --Input: drop 5 [1,2,3,4,5,6,7,8,9,10]
        --Output: [6,7,8,9,10]

        -- 0 1 2 3 4 5 6 7 8 9
        --[1,2,3,4,5,6,7,8,9,10]
        --subLista 5 7 -> [6,7,8]
        subLista :: (Eq a) => [a] -> Int -> Int -> [a]
        subLista lista desde hasta
            |desde == hasta = [lista!!hasta]
            |desde > hasta = error "indices invalidos"
            |otherwise = take (hasta-desde+1) (drop (desde) lista)
    
    
        --100(x(i+1) - xi2)2 + (1-xi)2
        
        rosenbrock :: [Double] -> Double
        rosenbrock lista
            |length lista < 2 = 0
        rosenbrock (x:y:resto) = 100*((y - x^2)^2) + (1-x)^2 + rosenbrock(y:resto)
  
        delta :: [Double] -> Int -> Double -> [Double]
        --subLista lista desde hasta
        delta [] _ _ = error "lista vacia"
        delta (primero:resto) posicion cambio
            |posicion<0 || posicion > (length (primero:resto)-1) = error "indice invalido"
            |posicion == 0 = ((primero+cambio):resto)       
        delta lista posicion cambio  
            |posicion == length(lista)-1 = (subLista lista 0 (posicion-1)) ++ [lista!!posicion+cambio]
            |otherwise = (subLista lista 0 (posicion-1)) ++ [(lista!!posicion+cambio)] ++ (subLista lista (posicion+1) (length (lista)))
    
        -- ++ :: [a] -> [a] -> [a]
        -- !! :: [a] -> a
        
        surroundings :: [Double] -> Double -> [[Double]]
        surroundings vector desplazamiento = (aux vector (-desplazamiento)) ++ (aux vector desplazamiento)
            where aux lista desplazamiento = [delta lista posicion desplazamiento | posicion <- [0.. (length lista)-1]]

        

        minRosenbrock :: [[Double]] -> (Double,[Double])
        minRosenbrock lista = (valorMinRos,listaMinRos) 
            where 
            listaRos = map rosenbrock lista
            valorMinRos = minimum listaRos
            listaMinRos = lista!!(posicionMin listaRos)

        
        posicionMin :: [Double] -> Int
        posicionMin lista = fromMaybe 0 (posicion)
            where
                minimo = minimum lista
                posicion = elemIndex minimo lista -- fromMaybe
    
    
        localMinimum :: [Double] -> Double -> Bool
        localMinimum lista desplazamiento = (rosenbrock lista) <= valorMin
            where
                entorno = surroundings lista desplazamiento
                valorMin = fst (minRosenbrock entorno)


        contadorIsEqual:: [Int] -> Int -> Int
        contadorIsEqual [] a = 0
        contadorIsEqual (x:[]) a
            |x==a = 1
            |otherwise = 0
        contadorIsEqual (x:xs) a
            |x == a = 1 + contadorIsEqual(xs) a
            |otherwise = 0 + contadorIsEqual(xs) a



        inRange :: Int -> Int -> Int -> Bool
        inRange min max n = (n <= max ) && (n >= min)
          


        isSorted:: [Int] -> Bool -> Bool
        isSorted [] condicion = True
        isSorted [_] condicion = True 
        isSorted (primero:segundo:resto) True
            |primero < segundo = isSorted(segundo:resto) True
            |primero > segundo = False
        isSorted (primero:segundo:resto) False
            |primero > segundo = isSorted(segundo:resto) False
            |primero < segundo = False




          --Eliminia dupliados
        distintos:: [Int] -> [Int]
        distintos[] = []
        distintos[a] = [a]
        distintos (x:xs)
            |elem x xs = distintos xs
            |otherwise = [x] ++ distintos xs

