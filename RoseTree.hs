module RoseTree where

data RoseTree t = Rose t [RoseTree t]
    deriving (Show, Eq)
