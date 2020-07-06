import Data.List

sinCerosALaIzquierda :: [Int] -> Bool
sinCerosALaIzquierda [x] = True
sinCerosALaIzquierda (x:y:xs)
   |x==0 && y/=0 = False
   |True = sinCerosALaIzquierda (y:xs)