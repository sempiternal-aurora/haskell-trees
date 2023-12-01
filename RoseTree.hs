module RoseTree where

data RoseTree t = Rose t [RoseTree t]
    deriving (Show, Eq)

instance Functor RoseTree where
    fmap f (Rose r rs) = Rose (f r) (map (fmap f) rs)

instance Foldable RoseTree where
    -- | Use the list foldable instance to fold over the children of the RoseTree
    foldr f z (Rose r rs) = r `f` foldr (flip (foldr f)) z rs

instance Traversable RoseTree where
    -- | The list traverse instance is extremely helpful here
    sequenceA (Rose r rs) = Rose <$> r <*> traverse sequenceA rs

    traverse f (Rose r rs)  = Rose <$> f r <*> traverse (traverse f) rs
