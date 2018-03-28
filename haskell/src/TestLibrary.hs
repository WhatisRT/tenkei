module TestLibrary (modifyArray, invertStringCase, exponentiate) where

import Data.Char

modifyArray :: [Int] -> [Int]
modifyArray = zipWith (*) [1..]

invertStringCase :: [Char] -> [Char]
invertStringCase = fmap invertCase

exponentiate :: Int -> Int -> Int
exponentiate x y = x^y

invertCase :: Char -> Char
invertCase c | isLower c = toUpper c
             | isUpper c = toLower c
             | otherwise = c
