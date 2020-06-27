module RoundUp
    (
    roundUp
    ) where

    
    -- 126.23 1

    -- 126.23*10 a la entero = 1262.3

    -- round(1262.3) =1262

    -- 1262 / 10 a al entero = 126.2

    roundUp :: Double -> Int -> Double   
    roundUp numero mover = (fromIntegral(round(numero*(10^mover))))/(10^mover)