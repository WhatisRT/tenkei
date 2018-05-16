module Main where

import Control.Monad
import Data.Int
import Data.Maybe
import System.Random
import System.TimeIt
import TreeTenkei
-- import TreeNative

randomFloat :: IO Float
randomFloat = randomIO

randomInt :: IO Int32
randomInt = randomIO

generateTree :: IO (Tree Int32)
generateTree = do
  isNil <- randomFloat
  decoration <- randomInt
  if isNil < 1/3
    then do
      left <- generateTree
      right <- generateTree
      return (Node decoration left right)
    else return Nil

times :: Int
times = 10000000

main :: IO ()
main = timeIt $ do
  trees <- replicateM times generateTree
  let minima = catMaybes $ fmap findMinimum trees
  print $ div (sum minima) $ fromIntegral times
  return ()
