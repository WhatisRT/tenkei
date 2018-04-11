module TestLibrary (libraryLanguage, modifyArray, exponentiate, identity, chooseLeft, reverseList) where

import Data.Int

libraryLanguage :: [Int32]
libraryLanguage = fmap (fromIntegral . fromEnum) "haskell"

modifyArray :: [Int32] -> [Int32]
modifyArray = zipWith (*) [1..]

exponentiate :: Int32 -> Int32 -> Int32
exponentiate x y = x^y

identity :: a -> a
identity x = x

chooseLeft :: a -> b -> a
chooseLeft x _ = x

reverseList :: [a] -> [a]
reverseList [] = []
reverseList (x : xs) = reverseList xs ++ [x]

--applyFunction :: (Int32 -> Int32) -> Int32 -> Int32
--applyFunction f = f
