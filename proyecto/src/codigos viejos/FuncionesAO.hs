module FuncionesAO
    ( 
    )
    where
        import Data.List 
        import Data.Maybe

        esEntero :: Double -> Bool
        esEntero n = fromIntegral(floor n) == n

        integers :: [Double] -> [Double]
        integers doubles = filter esEntero doubles

        integers2 :: [Double] -> [Int]
        integers2 doubles = map round (integers doubles) 

        clamp :: Int -> Int -> Int -> Int
        clamp mini maxi n
            |mini>maxi = error ""
            |n>maxi = maxi
            |n<mini = mini
        clamp _ _ n = n
        
        clampList:: [Int] -> Int -> Int -> [Int]
        clampList lista mini maxi = map (clamp mini maxi) lista

        map3 :: (a->b) -> (a,a,a) -> (b,b,b)
        map3 f (x,y,z) = (f x , f y , f z)
        
        
        map2 :: (a->b) -> (a,a) -> (b,b)
        map2 f (x,y) = (f x , f y)

        subLista :: (Eq a) => [a] -> Int -> Int -> [a]
        subLista lista desde hasta
            |desde == hasta = [lista!!hasta]
            |desde > hasta = error "indices invalidos"
            |otherwise = take (hasta-desde+1) (drop (desde) lista)

        subListasContiguas :: (Eq a) => [a] -> Int -> [[a]]
        subListasContiguas [] _ = []
        subListasContiguas (x:xs) largo
            |largo == 0 = error "!"
            |length (x:xs) < largo-1 = []
            |otherwise=(subLista (x:xs) 0 (largo-1)):(subListasContiguas xs largo )

        windowMap:: (Eq a) => [a] -> ([a]->a) -> Int -> [a]
        windowMap lista f n = map f listaDeListas 
            where 
                listaDeListas = subListasContiguas lista n
                 

        euclideanDistance :: [Double] -> [Double] -> Double
        euclideanDistance _   [] = error "!"
        euclideanDistance [] _ = error "!"
        euclideanDistance v1 v2 = sqrt (euclideanDistancesAUX v1 v2)
        

        euclideanDistancesAUX :: [Double] -> [Double] -> Double
        euclideanDistancesAUX [] [] = 0
        euclideanDistancesAUX (x:xs) (y:ys)
            |length (x:xs) /= length (y:ys) = error "!"
            |otherwise = (x-y)^2 + (euclideanDistancesAUX xs ys)
        
        hammingDistance :: (Eq a) => [a] -> [a] -> Double
        hammingDistance [] [] = 0
        hammingDistance [] (x:xs) = 1 + hammingDistance [] xs
        hammingDistance (x:xs) [] = 1 + hammingDistance xs []       
        hammingDistance (x:xs) (y:ys)
            |x/=y = 1 + hammingDistance xs ys
            |otherwise = hammingDistance xs ys

        diferentes :: (Eq a) => (a,a) -> Int
        diferentes (a,b)
            |a/=b = 1
            |otherwise = 0

        hammingDistance2 :: (Eq a) => [a] -> [a] -> Int
        hammingDistance2 v1 v2 = sum (map diferentes zipp)
            where
                zipp = zip v1 v2

        manhattanDistance :: [Double] -> [Double] -> Double
        manhattanDistance v1 v2
            |(length v1) /= (length v2) = 1.0/0.0
            |otherwise = sum (map (\(a,b) -> abs (a-b)) zipp)
            where
                zipp = zip v1 v2
        
        zip2 :: (Eq a) => [a] -> b -> [(a,b)]
        zip2 (x:[]) elem = [(x,elem)]
        zip2 (x:xs) elem = (x,elem):(zip2 xs elem)

        distanciaTupla :: ([Double] -> [Double] -> Double) -> ([Double],[Double]) -> Double
        distanciaTupla f (v1,v2) = f v1 v2

        nearest :: ([Double] -> [Double] -> Double) -> [Double] -> [[Double]] -> [Double]
        nearest f n lista = lista!!(posicionMin distancias)
            where
                zipp = zip2 lista n
                distancias = map (distanciaTupla f) zipp

        posicionMin :: [Double] -> Int
        posicionMin lista = fromMaybe 0 (posicion)
            where
                minimo = minimum lista
                posicion = elemIndex minimo lista -- fromMaybe

        type Point = [Double]
        type Points = [Point]
        type Distance = (Point -> Point -> Double)
        
        sacar :: (Eq a) => [a] -> a -> [a]
        sacar [] _ = []
        sacar (x:xs) el
            |x==el = xs
            |otherwise = x:(sacar xs el)

        knn :: Point -> Points -> Distance -> Int-> Points
        knn _ _ _ 0 = [[]]
        knn punto puntos distanciaFuncion nVeces = anterior:(knn punto (sacar puntos anterior) distanciaFuncion (nVeces-1))
            where
                anterior = nearest distanciaFuncion punto puntos 