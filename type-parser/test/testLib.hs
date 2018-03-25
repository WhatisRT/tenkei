{-# LANGUAGE ForeignFunctionInterface #-}

module TestModule where

import Foreign
import Foreign.C
import Foreign.Ptr
import System.IO.Unsafe

import Tenkei

foreign import ccall "triple" foreign_triple :: Ptr Word8 -> CSize -> Ptr (Ptr Word8) -> Ptr CSize -> IO ()
triple :: Int32 -> Int32
triple = unsafePerformIO . (call foreign_triple)

foreign import ccall "quadruple" foreign_quadruple :: Ptr Word8 -> CSize -> Ptr (Ptr Word8) -> Ptr CSize -> IO ()
quadruple :: Int32 -> Int32
quadruple = unsafePerformIO . (call foreign_quadruple)

foreign import ccall "quintuple" foreign_quintuple :: Ptr Word8 -> CSize -> Ptr (Ptr Word8) -> Ptr CSize -> IO ()
quintuple :: Int32 -> Int32
quintuple = unsafePerformIO . (call foreign_quintuple)


data TestType = TestType { test :: Int32, otherTest :: Int32 }
data TestType2 = Test Int32 | OtherTest Int32
