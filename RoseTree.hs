module RoseTree where

data RoseTree t = Rose t [RoseTree t]
    deriving (Show, Eq)

instance Functor RoseTree where
    fmap f (Rose r rs) = Rose (f r) (map (fmap f) rs)
