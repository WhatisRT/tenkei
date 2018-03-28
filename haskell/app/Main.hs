module Main where

import TestLibrary

main :: IO ()
main = do
  putStrLn $ show $ modifyArray [1..10]
  putStrLn $ invertStringCase "This IS a teST string"
