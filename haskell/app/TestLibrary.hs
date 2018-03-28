{-# LANGUAGE ForeignFunctionInterface #-}

module TestLibrary where

import Foreign
import Foreign.C

import Data.CBOR
import FFIWrappers
import Tenkei

foreign import ccall "tenkei_free" tenkei_free :: Ptr Word8 -> CSize -> IO ()

foreign import ccall "tenkei_modify_array" foreign_tenkei_modify_array :: Ptr Word8 -> CSize -> Ptr (Ptr Word8) -> Ptr CSize -> IO ()
modifyArray :: [Int] -> [Int]
modifyArray = call foreign_tenkei_modify_array tenkei_free

foreign import ccall "tenkei_invert_string_case" foreign_tenkei_invert_string_case :: Ptr Word8 -> CSize -> Ptr (Ptr Word8) -> Ptr CSize -> IO ()
invertStringCase :: [Char] -> [Char]
invertStringCase = call foreign_tenkei_invert_string_case tenkei_free

foreign import ccall "tenkei_exponentiate" foreign_tenkei_exponentiate :: Ptr Word8 -> CSize -> Ptr (Ptr Word8) -> Ptr CSize -> IO ()
exponentiate :: Int -> Int -> Int
exponentiate arg1 arg2 = deserialize $ callCBOR foreign_tenkei_exponentiate tenkei_free $ CBOR_Array [serialize arg1, serialize arg2]
