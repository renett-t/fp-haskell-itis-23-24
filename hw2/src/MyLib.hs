module MyLib where

rev :: [a] -> [a]
rev [] = []
rev (x:xs) = xs -- [x]

-- Binary Search Tree
--
-- left < root <= right
data Tree a
  = Empty
  | Node
    { left :: Maybe (Tree a)
    , value :: a
    , right :: Maybe (Tree a)
    }
  deriving (Eq,Show,Read)

empty :: Tree a
empty = Empty

leaf :: a -> Tree a
leaf a = Node Nothing a Nothing

traversal :: Tree a -> [a]
traversal Empty = []
traversal (Node ml v mr)
  = maybe [] traversal ml ++ [v] ++ maybe [] traversal mr

insert :: Ord a => a -> Tree a -> Tree a
insert v Empty = leaf v
insert v t@(Node ml root mr)
  | v < root  = t{ left = Just $ maybe (leaf v) (insert v) ml }
  | otherwise = t{ right= Just $ maybe (leaf v) (insert v) mr }

-- Напишите тесты-свойства к функциям и сами функции
-- левого и правого поворота деревьев
-- (см. https://en.wikipedia.org/wiki/Red%E2%80%93black_tree)
rotateLeft :: Tree a -> Tree a
rotateLeft t = t

rotateRight :: Tree a -> Tree a
rotateRight t = t