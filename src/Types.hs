module Types
    (Line,
    POINT,
    COLOR) where

-- IN ::= POINT COLOR(’\n’POINT COLOR)*
-- POINT ::= ’(’Int’,’Int’)’
-- COLOR ::= ’(’SHORT’,’SHORT’,’SHORT’)’
-- SHORT ::= ’0’..’255’

-- type COLOR = (Integer, Integer, Integer)
-- type POINT = (Int, Int)
-- type OUT = CLUSTER
-- type CLUSTER = "--\n" COLOR "\n -\n" (POINT COLOR "\n")

type POINT = (Integer, Integer)
type COLOR = (Double, Double, Double)

type Line = (POINT, COLOR)