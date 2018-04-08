module Main where

import TestLibrary
import Data.List

main :: IO ()
main =
  putStr $
  intercalate
    "\n"
    [ show $ modifyArray [1 .. 10]
    --, invertStringCase "This IS a teST string"
    , show $ fmap (exponentiate 2) [1 .. 20]
    , show $ identity "nice!"
    , show $ chooseLeft "Left" "Right"
    , show $ reverse "reverse this!"
    --, show $ applyFunction (*10) 5
    , ""
    ]
