module BinaryTree where

data BinaryTree a = Node (BinaryTree a) a (BinaryTree a) | Null
    deriving (Show, Eq)

instance Functor BinaryTree where
    fmap f (Node l x r) = Node (fmap f l) (f x) (fmap f r)
    fmap _ Null         = Null

instance Foldable BinaryTree where
    foldr f z (Node l x r) = foldr f (x `f` foldr f z r) l
    foldr _ z Null         = z

instance Traversable BinaryTree where
    sequenceA (Node l x r)  = Node <$> sequenceA l <*> x <*> sequenceA r
    sequenceA Null          = pure Null

    traverse f (Node l x r) = Node <$> traverse f l <*> f x <*> traverse f r
    traverse _ Null         = pure Null
