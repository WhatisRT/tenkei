{-# LANGUAGE DeriveGeneric #-}

module ExampleTypes where

import qualified GHC.Generics as GHC
import Generics.SOP
import Tenkei

data Test = Test1 Int | Test2 | Test3 | Test4 deriving (Show, GHC.Generic)
data Tree a = Leaf a | Node (Tree a) (Tree a) deriving (Show, GHC.Generic)

instance Generic Test
instance Tenkei Test

instance Generic (Tree a)
instance Tenkei a => Tenkei (Tree a)
