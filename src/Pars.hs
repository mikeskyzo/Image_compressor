module Pars
    (parsFile
    ) where

import Types

parsFile :: String -> LINES
parsFile file = do
    let allLines = split '\n' file
    loop allLines []
        where
            loop [] acc = acc
            loop (line:l) acc = loop l (acc ++ [(decrypt line)])

getNelem :: [String] -> Int -> String
getNelem l n
    | n < 0 = "0"
    | (length l) < (n - 1) = "0"
    | otherwise = l !! n

getNelemPoint :: [String] -> Int -> String
getNelemPoint l n
    | n < 0 = "(0,0)"
    | (length l) < (n - 1) = "(0,0)"
    | otherwise = l !! n

getNelemColor :: [String] -> Int -> String
getNelemColor l n
    | n < 0 = "(0,0,0)"
    | (length l) < (n - 1) = "(0,0,0)"
    | otherwise = l !! n

decrypt :: String -> LINE
decrypt line = do
    let allLines = split ' ' line
    ((getPoint (getNelemPoint allLines 0)), (getColor (getNelemColor allLines 1)))

getPoint :: String -> POINT
getPoint line = do
    let lineParsedList = split ',' (removeChar (removeChar line '(') ')')
    (read (getNelem lineParsedList 0) :: Integer, read (getNelem lineParsedList 1) :: Integer)

getColor :: String -> COLOR
getColor line = do
    let lineParsedList = split ',' (removeChar (removeChar line '(') ')')
    (read (getNelem lineParsedList 0) :: Double, read (getNelem lineParsedList 1) :: Double, read (getNelem lineParsedList 2) :: Double)

removeChar :: String -> Char -> String
removeChar str delimiter = loopIt str delimiter []
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