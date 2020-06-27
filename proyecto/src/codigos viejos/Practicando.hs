module Practicando
    ( distintos,
      isEscalera,
      inRange,
      isSorted,
      contadorIsEqual, 
      isFull,
      isPieza

    ) 
    where
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



          isEscalera:: [Int] -> Bool
          isEscalera lista
               |length lista /= 5 = error "la lista no tiene 5 numeros"
               |length (filter (<7) lista) /= 5 = False
               |length (filter (>0) lista) /= 5 = False 
               |isSorted lista True && ((lista!!0 == 1 && lista!!4==5) || (lista!!0 == 2 && lista!!4==6))=True
               |lista == [3..6]++[1]=True
               |otherwise =False


          isFull:: [Int] -> Bool
          isFull lista
               |length lista /= 5 = error "la lista no tiene 5 numeros"
               |length (filter (<7) lista) /= 5 = False
               |length (filter (>0) lista) /= 5 = False 
          isFull lista = resultado
               where
               comprension = [(contadorIsEqual lista n) | n <- [1.. length lista]]
               resultado = ((contadorIsEqual comprension 3 ) == 1) && ((contadorIsEqual comprension 2 ) == 1)



          isPieza::(Int, Int) -> (Int, Int) -> Bool
          isPieza (palo1, numero1) (palo2, numero2)
               |(inRange 1 4 palo1 && inRange 1 4 palo2) && (palo1 == palo2) && (elem numero2 [2,4,5,10,11])= True
               |otherwise = False

          -- nOf :: Int -> a -> [a]
          -- nOf n v
          --      |n<=0 = []
          --      |otherwise = v: nOf (n-1) v
          
          nOf :: Int -> a -> [a]
          nOf n v
               |n<=0 = []
               |otherwise = take n (repeat v)

          --Escribir una función de Haskell que tome una lista de enteros (no necesariamente ordenada) 
          --y retorne el menor número entero que no esté en la lista, pero que sea mayor al mínimo número de la lista. 
          --Si la lista está vacía se debe arrojar un error.
          --Por ejemplo:
          --(minAfterMin [1,2,3]) == 4 
          --(minAfterMin [1,3,7,5,6]) == 2
          --(minAfterMin [17]) == 18
          --(minAfterMin [77,700,707,70,7,770]) == 8
          --(minAfterMin [0..10]) == 11

          minAfterMin :: [Int] -> Int
          minAfterMin [] = error "la lista esta vacia"
          minAfterMin lista
               |not (elem (minimoDeLista+1) lista)= minimoDeLista+1
               |otherwise = minimoDeL (minimoDeLista+1)  lista
                    where
                         minimoDeLista = minimum lista
                         minimoDeL n lista 
                              |elem (n+1) lista = minimoDeL(n+1) lista
                              |otherwise= n+1

          updateTriplet :: (a,a,a) -> Int -> a -> (a,a,a)
          updateTriplet (x,y,z) posicion valor
               |posicion==1 = (valor,y,z)
               |posicion==2 = (x,valor,z)
               |posicion==3 = (x,y,valor)
               |otherwise = error "puto el que lo lee"
