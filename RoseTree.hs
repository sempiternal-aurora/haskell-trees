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

instance Applicative RoseTree where
    -- | Unlike the zip instance used for binary trees, we can be monadic and apply each function to create a rose tree
    --   and then collapse it down into a single RoseTree
    pure x = Rose x []

    (<*>) (Rose f frs) (Rose r rs) = Rose (f r) (fmap (fmap f) rs ++ zipWith (<*>) frs rs)

instance Monad RoseTree where
    (>>=) (Rose r rs) f = Rose r' (rs' ++ map (>>=f) rs)
        where Rose r' rs' = f r