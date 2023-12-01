module BinaryTree where

data BinaryTree a = Node (BinaryTree a) a (BinaryTree a) | Null
    deriving (Show, Eq)

instance Functor BinaryTree where
    fmap f (Node l x r) = Node (fmap f l) (f x) (fmap f r)
    fmap _ Null         = Null
