module Lib
    (distance
    ) where

-- SHORT ::= 0 ... 255
-- COLOR ::= (SHORT, SHORT, SHORT)
-- POINT ::= (Int, Int)
-- OUT ::= CLUSTER*
-- CLUSTER ::= "--\n" COLOR "\n -\n" (POINT COLOR "\n") *

-- type R = SHORT
-- type G = SHORT
-- type B = SHORT

-- data COLOR


get1st :: (Double, Double, Double) -> Double
get1st (x,_,_) = x

get2nd :: (Double, Double, Double) -> Double
get2nd (_,x,_) = x

get3rd :: (Double, Double, Double) -> Double
get3rd (_,_,x) = x

distance :: (Double, Double, Double) -> (Double, Double, Double) -> IO()
distance (x1, y1, z1) (x2, y2, z2) = print (((x2 - x1)*(x2 - x1) + (y2 - y1)*(y2 - y1) + (z2 - z1)*(z2 - z1))/2)
