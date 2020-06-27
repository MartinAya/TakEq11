module Practico1
(
    nextMonth,
    previousMonth,
    rockPaperScissors,
    fromInchestoFeet,
    fromYardstoFeet,
    isSorted,
    fromDirStr,
    toDirStr,
    traceDirStr,
    subListas,
    isGenerala,
    isEqual, 
    isPoker,
    contadorIsEqual
)
where
    
    nextMonth :: Int -> Int -> Int
    nextMonth mes cambio = (mod (mes + cambio)  12)

    previousMonth :: Int -> Int -> Int
    previousMonth mes cambio = (mod (mes - cambio) 12)

    -- 0 es piedra, 1 es papel y 2 es tijeras
    -- 1 si gana el primer jugador, -1 si gana el segundo jugador o 0 si es un empate

    rockPaperScissors :: Int -> Int -> Int
    rockPaperScissors j1 j2
        |j1==j2 = 0                         --caso empate
        |j1 == 0 = if(j2==1)then(-1)else(1)
        |j1 == 1 = if(j2==2)then(-1)else(1)
        |j1 == 2 = if(j2==0)then(-1)else(1)

    fromInchestoFeet:: Double -> Double
    fromInchestoFeet inches = inches/12
     
    fromYardstoFeet:: Double -> Double
    fromYardstoFeet yard = yard*3

    inRange :: Int -> Int -> Int -> Bool
    inRange min max n = (n <= max ) && (n >= min)

    fromDirChar:: Char -> Int
    fromDirChar char
        |char == '>' = 1
        |char == '<' = -1
        |otherwise = 0


    --(x:[]) lista con un solo  elemento
    isSorted:: [Int] -> Bool -> Bool
    isSorted [] condicion = True
    isSorted [_] condicion = True 
    isSorted (primero:segundo:resto) True
            |primero < segundo = isSorted(segundo:resto) True
            |primero > segundo = False
    isSorted (primero:segundo:resto) False
            |primero > segundo = isSorted(segundo:resto) False
            |primero < segundo = False

    
    --foldl1 (+) [1,2,3,4,5] = 15
    fromDirStr:: String -> Int 
    fromDirStr cadena = foldl1 (+) (map fromDirChar cadena)
       

    toDirStr:: Int -> String
    toDirStr n
        |n==0 = ""
        |n>0 = ">" ++ toDirStr(n-1)
        |n<0 = "<" ++ toDirStr(n+1)
    
    traceDirStr:: String -> [Int]  
    traceDirStr string = map fromDirStr (subListas string)

    subListas :: (Eq a) => [a] -> [[a]]
    subListas lista = [(take n lista) | n <- [1.. length lista]]

    isGenerala:: [Int] -> Bool
    isGenerala lista
        |(( length(lista)==5 ) && (inRange 1 6 (lista !! 0) )) = isEqual lista
        |otherwise = False
    
   --(Eq a) => [a] -> Bool
    
    isEqual:: [Int] -> Bool
    isEqual [] = True
    isEqual [_] = True
    isEqual (x:y:xs)
        |x == y = isEqual(y:xs)
        |otherwise = False

    isPoker :: [Int] -> Bool
    isPoker lista
        |length (filter (<7) lista) /= 5 = False
        |length (filter (>0) lista) /= 5 = False
    isPoker lista = resultado
        where 
        comprension = [(contadorIsEqual lista n) | n <- [1.. length lista]]
        resultado = (contadorIsEqual comprension 4 ) == 1
--	(a -> Bool) -> [a] -> [a]    tiene que tirar largo 5
-- Input: filter (>5) [-1,1,2,3,4]
-- Output: [6,7,8]

--- contadorIsEqual lista [1,1,1,1,1]
--  contadorIsEqual 4 

    contadorIsEqual:: [Int] -> Int -> Int
    contadorIsEqual [] a = 0
    contadorIsEqual (x:[]) a
        |x==a = 1
        |otherwise = 0
    contadorIsEqual (x:xs) a
        |x == a = 1 + contadorIsEqual(xs) a
        |otherwise = 0 + contadorIsEqual(xs) a