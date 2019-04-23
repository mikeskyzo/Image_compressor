module Pars
    (parsFile
    ) where

import Types

parsFile :: String -> [LINE]
parsFile file = do
    let lines = split '\n' file
    loop lines []
        where
            loop [] acc = acc
            loop (line:l) acc = loop l (acc ++ [(decrypt line)])

decrypt :: String -> LINE
decrypt line = do
    let lines = split ' ' line
    ((getPoint (lines !! 0)), (getColor (lines !! 1)))

getPoint :: String -> POINT
getPoint line = do
    let lineParsedList = split ',' (removeBracket (removeBracket line '(') ')')

    (read (lineParsedList !! 0) :: Integer, read (lineParsedList !! 1) :: Integer)

getColor :: String -> COLOR
getColor line = do
    let lineParsedList = split ',' (removeBracket (removeBracket line '(') ')')
    (read (lineParsedList !! 0) :: Double, read (lineParsedList !! 1) :: Double, read (lineParsedList !! 2) :: Double)

removeBracket :: String -> Char -> String
removeBracket str delimiter = loopIt str delimiter []
    where
        loopIt [] _ acc = acc
        loopIt (c:l) delim acc
            | c == delim = loopIt l delim acc
            | otherwise = loopIt l delim (acc ++ [c])

split :: Char -> String -> [String]
split _ [] = []
split c (x:xs) | x == c = split c xs
split c xs = w:(split c rest)
    where (w, rest) = break (==c) xs