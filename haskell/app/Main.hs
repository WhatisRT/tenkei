module Main where

import TestLibrary
import Data.List
import Data.Int

main :: IO ()
main =
  putStr $
  intercalate
    "\n"
    [ "haskell"
    , showLang libraryLanguage
    , show $ binaryOr True False
    , show $ modifyArray [1 .. 10]
    , show $ fmap (exponentiate 2) [1 .. 10]
    , show $ identity [1..10]
    , show $ chooseLeft 1 2
    , show $ reverse [1..10]
    , ""
    ]

showLang :: [Int32] -> String
showLang = fmap (toEnum . fromIntegral)
