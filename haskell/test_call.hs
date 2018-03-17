{-# LANGUAGE ForeignFunctionInterface #-}

import Foreign
import Foreign.C
import Foreign.Ptr

import Data.Maybe

import Text.Read

import Tenkei
import Rustlib

import System.Environment
import System.IO.Unsafe

main :: IO ()
main = do
  args <- getArgs
  putStrLn $ show $ sequence $ fmap ((fmap (unsafePerformIO . triple)) . readMaybe) args
