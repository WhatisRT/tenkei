module TreeNative where

data Tree a = Nil | Node a (Tree a) (Tree a)

flatten :: Tree a -> [a]
flatten Nil = []
flatten (Node x t1 t2) = flatten t1 ++ [x] ++ flatten t2

findMinimum :: (Ord a) => Tree a -> Maybe a
findMinimum =
  (\x ->
     case x of
       [] -> Nothing
       y -> Just $ minimum y) .
  flatten
