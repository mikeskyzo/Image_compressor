module Pars
    (parsFile
    ) where

import Types

parsFile :: String -> [LINE]
parsFile _ = ((1, 1), (10 ,20 ,30)):[]
