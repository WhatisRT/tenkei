module TestLibrary (modifyArray, invertStringCase, exponentiate, identity, chooseLeft, reverseList, applyFunction) where

import Data.Char

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

reverseList :: [a] -> [a]
reverseList [] = []
reverseList (x : xs) = reverseList xs ++ [x]

applyFunction :: (Int -> Int) -> Int -> Int
applyFunction f = f

invertCase :: Char -> Char
invertCase c | isLower c = toUpper c
             | isUpper c = toLower c
             | otherwise = c
