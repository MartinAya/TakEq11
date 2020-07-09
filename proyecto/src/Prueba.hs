import Data.List
import Data.Maybe
import Data.Char

takeHasta :: (Eq a) => [a] -> (a->Bool)-> [a]
--hace un take en la lista desde el principio hasta una posicion antes
--de la primera ocurrencia del primer elemento que cumpla con la condicion en f
takeHasta lista f
   |posicionPrimero == (-1) = lista
   |otherwise = take posicionPrimero lista
   where
      mapAf = map f lista
      posicionPrimero = fromMaybe (-1) (elemIndex True mapAf)