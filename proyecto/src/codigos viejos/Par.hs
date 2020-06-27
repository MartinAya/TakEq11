module Par
    ( 
    )
    where
        import Data.List 
        import Data.Maybe

       -- [1,2,3,4,5,6,7,8,9,10]
       -- subLista 5 7 -> [6,7,8]
        subLista :: (Eq a) => [a] -> Int -> Int -> [a]
        subLista lista desde hasta
            |desde == hasta = [lista!!hasta]
            |desde > hasta = error "indices invalidos"
            |otherwise = take (hasta-desde+1) (drop (desde) lista)

        -- posicionMin :: [Double] -> Int
        -- posicionMin lista = fromMaybe 0 (posicion)
        --     where
        --         minimo = minimum lista
        --         posicion = elemIndex minimo lista -- fromMaybe
        
        -- isEqual:: [Int] -> Bool
        -- isEqual [] = True
        -- isEqual [_] = True
        -- isEqual (x:y:xs)
        --     |x == y = isEqual(y:xs)
        --     |otherwise = False

        -- contadorIsEqual:: [Int] -> Int -> Int
        -- contadorIsEqual [] a = 0
        -- contadorIsEqual (x:[]) a
        --     |x==a = 1
        --     |otherwise = 0
        -- contadorIsEqual (x:xs) a
        --     |x == a = 1 + contadorIsEqual(xs) a
        --     |otherwise = 0 + contadorIsEqual(xs) a



        inRange :: Int -> Int -> Int -> Bool
        inRange min max n = (n <= max ) && (n >= min)
          


        -- isSorted:: [Int] -> Bool -> Bool
        -- isSorted [] condicion = True
        -- isSorted [_] condicion = True 
        -- isSorted (primero:segundo:resto) True
        --     |primero < segundo = isSorted(segundo:resto) True
        --     |primero > segundo = False
        -- isSorted (primero:segundo:resto) False
        --     |primero > segundo = isSorted(segundo:resto) False
        --     |primero < segundo = False


        --   --Eliminia dupliados
        -- distintos:: [Int] -> [Int]
        -- distintos[] = []
        -- distintos[a] = [a]
        -- distintos (x:xs)
        --     |elem x xs = distintos xs
        --     |otherwise = [x] ++ distintos xs

-- isValidRoll [1,1,1,2,2] = True
-- isValidRoll [3,4,3,4,3] = True
-- isValidRoll [5,5,5,5,5] = True
-- isValidRoll [3,2,1,0,6] = False
-- isValidRoll [3,4,5,6,7] = False
-- isValidRoll [1,2,3,4,5,6] = False
-- isValidRoll [1,2,3,4] = False
-- isValidRoll [] = False
        isValidRoll:: [Int] -> Bool
        isValidRoll lista
            |length (filter (<7) lista) /= 5 = False
            |length (filter (>0) lista) /= 5 = False
            |otherwise = True
        
        sortPair:: (Double, Double) -> (Double, Double)
        sortPair (x1, y1)= if(x1 <= y1) then (x1, y1) else (y1, x1)

--             '&' para el AND,
--     '|' para el OR,
--     '=' para el si y solo si, 
--     '>' para el condicional. 

-- Cualquier otro caracter debería provocar un error.

-- evalCond True '&' False = False
-- evalCond True '|' False = True
-- evalCond True '=' True = True
-- evalCond True '=' False = False
-- evalCond False '=' True = False
-- evalCond False '=' False = True
-- evalCond True '>' True = True
-- evalCond True '>' False = False
-- evalCond False '>' True = True
-- evalCond False '>' False = True
-- evalCond False '?' True = error "!"
        evalCond:: Bool -> Char -> Bool -> Bool
        evalCond   cond1 caract cond2
            |caract ==  '&' = (cond1 && cond2)
            |caract ==  '|' = (cond1 || cond2)
            |caract ==  '=' = (cond1 == cond2)
            |caract ==  '>' = if cond1 then cond2 else True
            |otherwise = error "!"

