module TestLibrary (modifyArray, invertStringCase, exponentiate, identity, chooseLeft) where

import Data.Char
import Tenkei

modifyArray :: [Int] -> [Int]
modifyArray = zipWith (*) [1..]

invertStringCase :: [Char] -> [Char]
invertStringCase = fmap invertCase

exponentiate :: Int -> Int -> Int
exponentiate x y = x^y

identity :: a -> a
identity x = x

chooseLeft :: a -> b -> a
chooseLeft x _ = x

invertCase :: Char -> Char
invertCase c | isLower c = toUpper c
             | isUpper c = toLower c
             | otherwise = c
