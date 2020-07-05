module Prueba()
where

subLista :: (Eq a) => [a] -> Int -> Int -> [a]
subLista lista desde hasta
      |desde == hasta = [lista!!hasta]
      |desde > hasta = error "indices invalidos"
      |otherwise = take (hasta-desde+1) (drop (desde) lista)

setLista :: (Eq a) => [a] -> Int -> a -> [a]
setLista lista lugar ele = [if x==lugar then ele else lista!!x | x<-[0..(length lista - 1)]]


setListaN :: (Eq a) => [a] -> [Int] -> [a] -> [a]
--lista original, lista posiciones, lista de nuevos elementos -> nueva lista
--(posicion,elemento)
setListaN  lista [] [] = lista
setListaN  lista (x:xs) (y:ys) = setListaN (setLista lista x y) xs ys