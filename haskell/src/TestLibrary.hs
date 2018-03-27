module TestLibrary (modifyArray, invertStringCase) where

import Data.Char

modifyArray :: [Int] -> [Int]
modifyArray = zipWith (*) [1..]

invertStringCase :: [Char] -> [Char]
invertStringCase = fmap invertCase

invertCase :: Char -> Char
invertCase c | isLower c = toUpper c
             | isUpper c = toLower c
             | otherwise = c
