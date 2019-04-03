module Algo
    (algo
    ) where

import Types

algo :: Integer -> Float -> [Line] -> String
algo _ _ _ = "prout"

distance :: COLOR -> COLOR -> Double
distance (x1, y1, z1) (x2, y2, z2) = sqrt (((x2 - x1)^2) + ((y2 - y1)^2) + ((z2 - z1)^2))

closest :: COLOR -> [COLOR] -> COLOR
closest point list = loop point list (-1, -1, -1)
    where
        loop _ [] c = c
        loop p (l:end) (-1, -1, -1) = loop p end (findClose p l l)
        loop p (l:end) c = loop p end (findClose p l c)

findClose :: COLOR -> COLOR -> COLOR -> COLOR
findClose p p1 p2 = do
    let disP1 = distance p p1
    let disP2 = distance p p2
    retClose p1 p2 disP1 disP2

retClose :: COLOR -> COLOR -> Double -> Double -> COLOR
retClose p1 p2 disP1 disP2
    | disP1 > disP2 = p2
    | disP1 < disP2 = p1
    | otherwise = p1


newPoint :: [COLOR] -> COLOR
newPoint l = calcMoy l (0, 0, 0) 0
    where
        calcMoy [] (r, g, b) acc = (r / acc, g / acc, b / acc)
        calcMoy ((r, g, b):l) (r1, g1, b1) acc = calcMoy l ((r + r1), (g + g1), (b + b1)) (acc + 1)

convergence :: COLOR -> COLOR -> Double
convergence a b = distance a b
