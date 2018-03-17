{-# LANGUAGE ForeignFunctionInterface #-}

module Rustlib where

import Foreign
import Foreign.C
import Foreign.Ptr

import Tenkei

foreign import ccall "triple" foreign_triple :: Ptr Word8 -> CSize -> Ptr (Ptr Word8) -> Ptr CSize -> IO ()
triple :: Int32 -> IO Int32
triple = call foreign_triple



