module Algo
    (algo,
    ) where

import Types
import System.Random

-- main algo
algo :: Int -> Double -> LINES -> String
algo nb convLimit allLines
    | nb <= 0 = "ERROR : can't have zero or less clusters\n"
    | convLimit < 0 = "ERROR : can't have a negative converge's limit\n"
    | otherwise = convertClusterToString (mainLoop nb convLimit allLines)

mainLoop :: Int -> Double -> LINES -> CLUSTERS
mainLoop nbCluster e l = do
    let clus = sortLine nbCluster l
    loopIt clus l e (putLinesInClusters (changeCluster clus) l)
        where
            loopIt oldClusters allLines convergeLimit newClusters
                | (isConverge oldClusters newClusters convergeLimit) == True = newClusters
                | otherwise = loopIt newClusters allLines convergeLimit (putLinesInClusters (changeCluster newClusters) allLines)


-- Sort all lines in closest clusters

sortLine :: Int -> LINES -> CLUSTERS
sortLine nbCluster allLines = do
    let clusters = creatClusters nbCluster
    putLinesInClusters clusters allLines

putLinesInClusters :: CLUSTERS -> LINES -> CLUSTERS
putLinesInClusters c [] = c
putLinesInClusters clusters (line:l) = putLinesInClusters (putLineInClosestCluster clusters line) l

putLineInClosestCluster :: CLUSTERS -> LINE -> CLUSTERS
putLineInClosestCluster clusters line = putLineInClusters clusters (closestCluster clusters line) line

closestCluster :: CLUSTERS -> LINE -> COLOR
closestCluster c (_, color) = closeCluster c color (-1, -1, -1)
    where
        closeCluster [] _ closestColor = closestColor
        closeCluster ((co, _):l) line (-1, -1, -1) = closeCluster l line co
        closeCluster ((co, _):l) line closestColor = closeCluster l line (findClose line closestColor co)

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

putLineInClusters :: CLUSTERS -> COLOR -> LINE -> CLUSTERS
putLineInClusters c color line = fmap (findClusters line color) c

findClusters :: LINE -> COLOR -> CLUSTER -> CLUSTER
findClusters line color cluster
    | (getClusterColor cluster) == color = ((getClusterColor cluster), (getClusterLines cluster) ++ [line])
    | otherwise = cluster

getClusterColor :: CLUSTER -> COLOR
getClusterColor (c, _) = c

getClusterLines :: CLUSTER -> LINES
getClusterLines (_, l) = l


-- Distance funct
distance :: COLOR -> COLOR -> Double
distance (x1, y1, z1) (x2, y2, z2) = sqrt (((x2 - x1)**2) + ((y2 - y1)**2) + ((z2 - z1)**2.0))

-- Check if two clusters converging according to the convergence limit given as parameter
isConverge :: CLUSTERS -> CLUSTERS -> Double -> Bool
isConverge [] _ _ = False
isConverge _ [] _ = False
isConverge ((c1, _):l1) ((c2, _):l2) convergeLimit
    | (distance c1 c2) < convergeLimit = True
    | otherwise = isConverge l1 l2 convergeLimit


-- Clusters funct (create, calc new cluster)
creatClusters :: Int -> CLUSTERS    -- Create N clusters empty
creatClusters nb = createIt nb []
    where
        createIt 0 acc = acc
        createIt n acc = do
            let seed = mkStdGen (n + 42)
            let numbers = take 3 $ randomRs (0, 255) seed :: [Double]
            createIt (n - 1) (((numbers !! 0, numbers !! 1, numbers !! 2), []):acc)

-- Change all cluster by they lines -> return list of clusters with no lines
changeCluster :: CLUSTERS -> CLUSTERS
changeCluster clusters = fmap changePointCluster clusters

-- Change the point of one cluster according to these lines
changePointCluster :: CLUSTER -> CLUSTER
changePointCluster (c, []) = (c, [])
changePointCluster (_, clusterlines) = ((newPoint clusterlines), [])

-- Calculate a new point according to the LINES
newPoint :: LINES -> COLOR
newPoint clusterLines = calcMoy clusterLines (0, 0, 0) 0
    where
        calcMoy [] (r, g, b) acc = (r / acc, g / acc, b / acc)
        calcMoy ((_,(r, g, b)):list) (r1, g1, b1) acc = calcMoy list ((r + r1), (g + g1), (b + b1)) (acc + 1)


-- Convert the Clusters and there lines in String
convertClusterToString :: CLUSTERS -> String
convertClusterToString clusters = loopCluster clusters ""
    where
        loopCluster [] s = s
        loopCluster (((a, b, c), clusterLines):l) s = loopCluster l (s ++ "--\n(" ++ (show a) ++ "," ++ (show b) ++ "," ++ (show c) ++ ")\n-\n" ++ (addLines clusterLines ""))
        addLines [] s = s
        addLines (((x, y), (a, b, c)):l) s = addLines l (s ++ "(" ++ (show x) ++ "," ++ (show y) ++ ") (" ++ (show (toInt a)) ++ "," ++ (show (toInt b)) ++ "," ++ (show (toInt c)) ++ ")\n")

toInt :: Double -> Int
toInt i = round i