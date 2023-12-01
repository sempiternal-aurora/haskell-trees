module BinaryTree where

data BinaryTree a = Node (BinaryTree a) a (BinaryTree a) | Null
    deriving (Show, Eq)
