module Pars
    (parsFile
    ) where

import Types

parsFile :: String -> [LINE]
parsFile file = do
    let lines = split file '\n'
    loop line []
        where
            loop [] acc = acc
            loop (line:l) acc = loop l (acc ++ (decrypt line))

decrypt :: String -> LINE
decrypt line = ((getPoint line), (getColor line))