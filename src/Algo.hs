module Algo
    (algo
    ) where

import Types
import System.Random

algo :: Integer -> Double -> [LINE] -> String
algo nb _ _
    | nb <= 0 = "ERROR : can't have null or negative numbers of clusters"
    | otherwise = "prout"

-- sortLine :: Integer -> [LINE] -> [CLUSTER]
-- sortLine nbCluster LINEs = do
--     clusters <- creatClusters nbCluster
--     putLinesInClusters clusters lines

putLinesInClusters :: CLUSTERS -> LINES -> [CLUSTER]
putLinesInClusters c [] = c
putLinesInClusters clusters (line:l) = putLinesInClusters (putLineInClosestCluster clusters line) l

putLineInClosestCluster :: CLUSTERS -> LINE -> CLUSTERS
putLineInClosestCluster clusters line = putLineInClusters clusters (closestCluster clusters line) line

putLineInClusters :: CLUSTERS -> COLOR -> LINE -> CLUSTERS
putLineInClusters c color line = fmap (funct line color) c

funct :: LINE -> COLOR -> CLUSTER -> CLUSTER
funct line color cluster
    | (getClusterColor cluster) == color = ((getClusterColor cluster), getClusterLines cluster ++ [line])
    | otherwise = cluster

getClusterColor :: CLUSTER -> COLOR
getClusterColor (c, _) = c

getClusterLines :: CLUSTER -> LINES
getClusterLines (_, l) = l

closestCluster :: CLUSTERS -> LINE -> COLOR
closestCluster c (_, color) = closeCluster c color (-1, -1, -1)
    where
        closeCluster [] _ closestColor = closestColor
        closeCluster ((co, _):l) line (-1, -1, -1) = closeCluster l line co
        closeCluster ((co, _):l) line closestColor = closeCluster l line (findClose line closestColor co)

distance :: COLOR -> COLOR -> Double
distance (x1, y1, z1) (x2, y2, z2) = sqrt (((x2 - x1)^2) + ((y2 - y1)^2) + ((z2 - z1)^2))

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

-- closest :: Line -> [CLUSTER] -> [CLUSTER]
-- closest line list = loop line list []
--     where
--         loop _ [] acc = acc
--         loop p (l:end) (-1, -1, -1) = loop p end (findClose p l l)
--         loop p (l:end) acc = loop p end (findClose p l acc)

newPoint :: [COLOR] -> COLOR
newPoint l = calcMoy l (0, 0, 0) 0
    where
        calcMoy [] (r, g, b) acc = (r / acc, g / acc, b / acc)
        calcMoy ((r, g, b):l) (r1, g1, b1) acc = calcMoy l ((r + r1), (g + g1), (b + b1)) (acc + 1)

convergence :: COLOR -> COLOR -> Double -> Bool
convergence a b convLimit
    | (distance a b) < convLimit = True
    | otherwise = False

creatClusters :: Int -> [CLUSTER]
creatClusters nb = createIt nb []
    where
        createIt 0 acc = acc
        createIt n acc = do
            let seed = mkStdGen (n + 42)
            let numbers = take 3 $ randomRs (0, 255) seed :: [Double]
            createIt (n - 1) (((numbers !! 0, numbers !! 1, numbers !! 2), []):acc)

