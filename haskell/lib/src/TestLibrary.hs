module TestLibrary
  ( libraryLanguage
  , binaryOr
  , modifyArray
  , exponentiate
  , identity
  , chooseLeft
  , reverseList
  , applyFunction
  ) where

import Data.Int

libraryLanguage :: [Char]
libraryLanguage = "haskell"

binaryOr :: Bool -> Bool -> Bool
binaryOr = (||)

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

applyFunction :: (a -> b) -> a -> b
applyFunction = ($)
