{-# LANGUAGE DeriveGeneric #-}

module ExampleTypes where

import qualified GHC.Generics as GHC
import Generics.SOP
import Tenkei

data Test = Test1 Int | Test2 | Test3 | Test4 deriving (Show, GHC.Generic)

instance Generic Test
instance Tenkei Test
