{-# LANGUAGE ForeignFunctionInterface #-}

module TestLibrary where

import Foreign
import Foreign.C

import Data.CBOR
import FFIWrappers
import System.IO.Unsafe
import Tenkei

foreign import ccall "tenkei_free" tenkei_free :: Ptr Word8 -> CSize -> IO ()

foreign import ccall "tenkei_modify_array" foreign_tenkei_modify_array :: Ptr Word8 -> CSize -> Ptr (Ptr Word8) -> Ptr CSize -> IO ()
modifyArray :: [Int] -> [Int]
modifyArray arg1 = call foreign_tenkei_modify_array tenkei_free arg1

foreign import ccall "tenkei_invert_string_case" foreign_tenkei_invert_string_case :: Ptr Word8 -> CSize -> Ptr (Ptr Word8) -> Ptr CSize -> IO ()
invertStringCase :: [Char] -> [Char]
invertStringCase arg1 = call foreign_tenkei_invert_string_case tenkei_free arg1

foreign import ccall "tenkei_exponentiate" foreign_tenkei_exponentiate :: Ptr Word8 -> CSize -> Ptr (Ptr Word8) -> Ptr CSize -> IO ()
exponentiate :: Int -> Int -> Int
exponentiate arg1 arg2 = deserialize $ callCBOR foreign_tenkei_exponentiate tenkei_free $ CBOR_Array [serialize arg1, serialize arg2]

foreign import ccall "tenkei_identity" foreign_tenkei_identity :: Ptr Word8 -> CSize -> Ptr (Ptr Word8) -> Ptr CSize -> IO ()
identity :: a -> a
identity arg1 = unsafePerformIO $ do
  arg1' <- toPointer arg1
  fromPointer $ call foreign_tenkei_identity tenkei_free arg1'

foreign import ccall "tenkei_choose_left" foreign_tenkei_choose_left :: Ptr Word8 -> CSize -> Ptr (Ptr Word8) -> Ptr CSize -> IO ()
chooseLeft :: a -> b -> a
chooseLeft arg1 arg2 = unsafePerformIO $ do
  arg1' <- toPointer arg1
  arg2' <- toPointer arg2
  fromPointer $ deserialize $ callCBOR foreign_tenkei_choose_left tenkei_free $ CBOR_Array [serialize arg1', serialize arg2']

