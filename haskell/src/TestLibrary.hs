module TestLibrary (modifyArray, invertStringCase, exponentiate, identity, chooseLeft, reverseList, applyFunction) where

import Data.Char
import Data.Int

modifyArray :: [Int32] -> [Int32]
modifyArray = zipWith (*) [1..]

invertStringCase :: [Char] -> [Char]
invertStringCase = fmap invertCase

exponentiate :: Int32 -> Int32 -> Int32
exponentiate x y = x^y

identity :: a -> a
identity x = x

chooseLeft :: a -> b -> a
chooseLeft x _ = x

reverseList :: [a] -> [a]
reverseList [] = []
reverseList (x : xs) = reverseList xs ++ [x]

applyFunction :: (Int32 -> Int32) -> Int32 -> Int32
applyFunction f = f

invertCase :: Char -> Char
invertCase c | isLower c = toUpper c
             | isUpper c = toLower c
             | otherwise = c
