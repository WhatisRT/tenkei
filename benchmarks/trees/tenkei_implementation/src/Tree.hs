{-# LANGUAGE DeriveGeneric #-}

module Tree where

import Data.Int
import Generics.SOP hiding (Nil)
import Tenkei
import qualified GHC.Generics as GHC

data Tree a = Nil | Node a (Tree a) (Tree a) deriving (GHC.Generic)

instance Generic (Tree a)

instance Tenkei a => Tenkei (Tree a)

flatten :: Tree a -> [a]
flatten Nil = []
flatten (Node x t1 t2) = flatten t1 ++ [x] ++ flatten t2

-- findMinimum :: (Ord a) => Tree a -> Maybe a
findMinimum :: Tree Int32 -> Maybe Int32
findMinimum =
  (\x ->
     case x of
       [] -> Nothing
       y -> Just $ minimum y) .
  flatten
