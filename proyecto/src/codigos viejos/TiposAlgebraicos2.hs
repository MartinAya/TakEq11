module TiposAlgebraicos
    ( 
    )
    where
        import Data.List 
        import Data.Maybe

        data Horoscopo = Aries | Tauro | Geminis | Cancer | Leo | Virgo | Libra | Escorpio | Sagitario | Capricornio | Acuario | Piscis
            deriving(Eq, Ord , Show, Read, Bounded, Enum)
     
        data Month = Marzo | Abril | Mayo | Junio | Julio | Agosto | Setiembre | Octubre | Noviembre | Diciembre | Enero | Febrero
            deriving (Eq , Ord , Show, Read, Bounded, Enum)

        type Date = (Int, Month)

        startDate :: Horoscopo -> Date
        startDate Aries = (21,Marzo)
        startDate Tauro = (20,Abril)
        startDate Geminis = (21,Mayo)
        startDate Cancer = (21,Junio)
        startDate Leo = (23,Julio)
        startDate Virgo = (23,Agosto)
        startDate Libra = (23,Setiembre)
        startDate Escorpio = (23,Octubre)
        startDate Sagitario = (22,Noviembre)
        startDate Capricornio = (22,Diciembre)
        startDate Acuario = (20,Enero)
        startDate Piscis = (19,Febrero)
        
        endDate :: Horoscopo -> Date
        endDate signo =  (dia -1,mes)
            where 
                signoSig = succ signo
                (dia,mes) = startDate signoSig

        data UKWeight = Stones Double | Pounds Double | Ounces Double
        --sacamos deriving Show y Eq
        

-- instance Eq Bool where
-- True == True = True
-- False == False = True
-- == _ = False
      
      -- a -> -> Bool

        ukq :: UKWeight -> Double
        ukq (Stones q1) = q1
        ukq (Pounds q1) = q1
        ukq (Ounces q1) = q1

        instance Show UKWeight where
            show (Pounds qty) = (show qty) ++ "lbs"
            show (Stones qty) = (show qty) ++ "sts"
            show (Ounces qty) = (show qty) ++ "oz"

        instance Eq UKWeight where
            (Stones q1) == uk2 = abs (q1 - ukq (toStones uk2)) < 0.00001
            (Pounds q1) == uk2 = abs (q1 - ukq (toPounds uk2)) < 0.00001
            (Ounces q1) == uk2 = abs (q1 - ukq (toOunces uk2)) < 0.00001
            
        instance Num UKWeight where
            (Stones q1) + uk2 = (Stones (q1 + ukq (toStones uk2)))
            (Pounds q1) + uk2 = (Pounds (q1 + ukq (toPounds uk2)))
            (Ounces q1) + uk2 = (Ounces (q1 + ukq (toOunces uk2)))
            (Stones q1) - uk2 = (Stones (q1 - ukq (toStones uk2)))
            (Pounds q1) - uk2 = (Pounds (q1 - ukq (toPounds uk2)))
            (Ounces q1) - uk2 = (Stones (q1 - ukq (toOunces uk2)))      
            abs (Stones q1) = if q1 <0 then (Stones (-q1)) else (Stones q1)
            abs (Pounds q1) = if q1 <0 then (Pounds (-q1)) else (Pounds q1)
            abs (Ounces q1) = if q1 <0 then (Ounces(-q1)) else (Ounces q1)

            (Stones q1) * uk2 =  Stones (q1 * (ukq (toStones uk2)))
            (Pounds q1) * uk2 =  Pounds (q1 * (ukq (toPounds uk2)))
            (Ounces q1) * uk2 =  Ounces (q1 * (ukq (toOunces uk2)))
            
            fromInteger n = (Pounds (fromInteger n))

            signum (Stones q1) = if q1<0 then (Stones (-1.0)) else if q1>0 then (Stones(1.0)) else (Stones (0.0))
            signum (Ounces q1) = if q1<0 then (Ounces (-1.0)) else if q1>0 then (Ounces (1.0)) else (Ounces(0.0))
            signum (Pounds q1) = if q1<0 then (Pounds (-1.0)) else if q1>0 then (Pounds (1.0)) else (Pounds (0.0))
        
        toOunces:: UKWeight -> UKWeight
        toOunces (Ounces q) = (Ounces q)
        toOunces (Pounds q) = (Ounces (q * 16))
        toOunces (Stones q) = (Ounces (q * 224))


        toPounds:: UKWeight -> UKWeight
        toPounds (Pounds q) = (Pounds q)
        toPounds (Ounces q) = (Pounds (q /16))
        toPounds (Stones q) = (Pounds (q * 14))
        
        
        toStones:: UKWeight -> UKWeight
        toStones (Stones q) = (Stones q)
        toStones (Ounces q) = (Stones (q/224))
        toStones (Pounds q) = (Stones (q/14))
        


        data Tree a = Leaf a | Node a (Tree a)(Tree a)| Empty
            deriving(Eq,Show)

        inOrder :: Tree a -> [a]
        inOrder Empty = []
        inOrder (Leaf a) = [a]
        inOrder (Node x tl t2) = (inOrder tl) ++ [x] ++ (inOrder t2)

        preOrder :: Tree a -> [a]
        preOrder Empty = []
        preOrder (Leaf a) = [a]
        preOrder (Node x tl t2) = [x] ++ (preOrder tl) ++ (preOrder t2)

        postOrder :: Tree a -> [a]
        postOrder Empty = []
        postOrder (Leaf a) = [a]
        postOrder (Node x tl t2) = (postOrder tl) ++ (postOrder t2) ++ [x]


        binTreeSearch :: (Ord a) => (Tree a) -> a -> Bool        
        binTreeSearch (Leaf valor) busc = valor==busc
        binTreeSearch Empty _ = False
        binTreeSearch (Node a izq der) valor
            |valor<a = binTreeSearch izq valor
            |valor>a = binTreeSearch der valor
            |valor==a = True
    

        data Naipe  = Oro Int | Copa Int | Espada Int | Basto Int
            deriving(Show,Eq)

        suitNaipe , numberNaipe :: Naipe -> Int
        suitNaipe (Basto n) = 4
        suitNaipe (Espada n) = 3
        suitNaipe (Copa n) = 2
        suitNaipe (Oro n) = 1
        numberNaipe (Basto n) = n
        numberNaipe (Espada n) = n
        numberNaipe (Copa n) = n
        numberNaipe (Oro n) = n

        baraja :: [Naipe]
        baraja = [s n | s <- [Oro,Copa,Espada,Basto], n <- [1..12]]

        orden :: Int -> Int
        orden 3 = 10
        orden 2 = 9
        orden 1 = 8
        orden 12 = 7
        orden 11 = 6
        orden 10 = 5
        orden 7 = 4
        orden 6 = 3
        orden 5 = 2
        orden 4 = 1

        valor :: Naipe -> Int
        -- 3 2 1 12 11 10 7 6 5 4
        valor (Espada n) = orden n
        valor (Copa n) = orden n
        valor (Oro n) = orden n
        valor (Basto n) = orden n

        beats :: Naipe -> Naipe -> Bool
        beats (Espada 1) _ = True
        beats  _ (Espada 1)= False
        beats (Basto 1) _ = True
        beats  _ (Basto 1) = False
        beats (Espada 7) _ = True
        beats  _ (Espada 7) = False
        beats (Oro 7) _ = True
        beats  _ (Oro 7) = False
        beats n1 n2 = (valor n1) > (valor n2)

        powerset[]=[[]]
        powerset(x:xs)=p++(map(x:)p)
            where p= powerset xs

        -- turnoEscoba15::[Naipe]->Naipe->[Naipe]
        -- turnoEscoba15 lista naipe
        --     | null == combs = lista:naipe 
        --     | otherwise combs
        --         where combs = [c|c <- powerset lista, sum(map numberNaipe (naipe:c))==15]
        turnoEscoba15:: [Naipe] -> Naipe -> [Naipe]
        turnoEscoba15 [] x = [x]
        turnoEscoba15 xs x = if length(quedaron xs x) == 0 then x:xs else xs \\ head (quedaron xs x)  
            where
                quedaron xs x = (filter (\y -> (sum (map numberNaipe y) == 15)) (sublists (x:xs)))


        sublists:: [a] -> [[a]]
        sublists [] = [[]]  
        sublists (x:xs) = [x:sublist | sublist <- sublists xs] ++ sublists xs

        data Optimization = Minimize | Maximize | Approximate Double
            deriving (Show, Eq)


        --approximate double -> quiero el valor que tire el mas cerca al valor funcional que quiero

        optimum1 :: Optimization -> ([Double] -> Double) -> [[Double]] -> (Double,[Double])
        optimum1 Minimize fu v = (valorMin,v!!posicionMin)
            where   
                mapf = map fu v
                valorMin = minimum mapf
                posicionMin = fromMaybe (-1) (elemIndex valorMin mapf)
        optimum1 Maximize fu v = (valorMax, v!!posicionMax)
            where
                mapf = map fu v
                valorMax = maximum mapf
                posicionMax = fromMaybe (-1) (elemIndex valorMax mapf)
        optimum1 (Approximate vf) fu v =  (valorAp, v!!posicionAp)      
            where
                mapf = map fu v
                mapd = map (distancia vf) mapf
                valorAp = minimum mapd
                posicionAp = fromMaybe (-1) (elemIndex valorAp mapd)
        
        distancia :: Double -> Double -> Double
        distancia a b = abs(a -b)

        localOptimum :: Optimization -> ([Double] -> Double) -> [Double] -> Double -> Bool
        localOptimum Minimize f v d = (snd optimo) == v
                where
                    vectoresLocales = surroundings v d
                    optimo = optimum1 Minimize f vectoresLocales
        localOptimum Maximize f v d = (snd optimo) == v
                where
                    vectoresLocales = surroundings v d
                    optimo = optimum1 Maximize f vectoresLocales
        
        hillClimbing :: Optimization -> ([Double] -> Double) -> [Double] -> Double -> [([Double],Double)]
        hillClimbing Minimize f v d
            |optimo==v = [(v, f v)]
            |otherwise = [(v, f v)]++(hillClimbing Minimize f optimo d)
                where
                    vectoresLocales = surroundings v d 
                    optimo = snd (optimum1 Minimize f vectoresLocales)
        hillClimbing Maximize f v d
            |optimo==v = [(v, f v)]
            |otherwise = [(v, f v)]++(hillClimbing Maximize f optimo d)
                where
                    vectoresLocales = surroundings v d 
                    optimo = snd (optimum1 Maximize f vectoresLocales)

        subLista :: (Eq a) => [a] -> Int -> Int -> [a]
        subLista lista desde hasta
            |desde == hasta = [lista!!hasta]
            |desde > hasta = error "indices invalidos"
            |otherwise = take (hasta-desde+1) (drop (desde) lista)
    
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

        