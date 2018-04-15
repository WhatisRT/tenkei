{-# LANGUAGE ForeignFunctionInterface #-}

module TestLibrary where

import Foreign
import Foreign.C

import Data.CBOR
import FFIWrappers
import System.IO.Unsafe
import Pointers
import Tenkei

foreign import ccall "tenkei_free" tenkei_free :: Ptr Word8 -> CSize -> IO ()

foreign import ccall "tenkei_library_language" foreign_tenkei_library_language :: Ptr Word8 -> CSize -> Ptr (Ptr Word8) -> Ptr CSize -> IO ()
libraryLanguage :: [Int32]
libraryLanguage  = unsafePerformIO $ do
  return $ deserialize $ callCBOR foreign_tenkei_library_language tenkei_free $ CBOR_Array []

foreign import ccall "tenkei_binary_or" foreign_tenkei_binary_or :: Ptr Word8 -> CSize -> Ptr (Ptr Word8) -> Ptr CSize -> IO ()
binaryOr :: Bool -> Bool -> Bool
binaryOr arg1 arg2 = unsafePerformIO $ do
  let arg1' = arg1
  let arg2' = arg2
  return $ deserialize $ callCBOR foreign_tenkei_binary_or tenkei_free $ CBOR_Array [serialize arg1', serialize arg2']

foreign import ccall "tenkei_modify_array" foreign_tenkei_modify_array :: Ptr Word8 -> CSize -> Ptr (Ptr Word8) -> Ptr CSize -> IO ()
modifyArray :: [Int32] -> [Int32]
modifyArray arg1 = unsafePerformIO $ do
  let arg1' = arg1
  return $ deserialize $ callCBOR foreign_tenkei_modify_array tenkei_free $ CBOR_Array [serialize arg1']

foreign import ccall "tenkei_exponentiate" foreign_tenkei_exponentiate :: Ptr Word8 -> CSize -> Ptr (Ptr Word8) -> Ptr CSize -> IO ()
exponentiate :: Int32 -> Int32 -> Int32
exponentiate arg1 arg2 = unsafePerformIO $ do
  let arg1' = arg1
  let arg2' = arg2
  return $ deserialize $ callCBOR foreign_tenkei_exponentiate tenkei_free $ CBOR_Array [serialize arg1', serialize arg2']

foreign import ccall "tenkei_identity" foreign_tenkei_identity :: Ptr Word8 -> CSize -> Ptr (Ptr Word8) -> Ptr CSize -> IO ()
identity :: (Tenkei a) => a -> a
identity arg1 = unsafePerformIO $ do
  let arg1' = arg1
  return $ deserialize $ callCBOR foreign_tenkei_identity tenkei_free $ CBOR_Array [serialize arg1']

foreign import ccall "tenkei_choose_left" foreign_tenkei_choose_left :: Ptr Word8 -> CSize -> Ptr (Ptr Word8) -> Ptr CSize -> IO ()
chooseLeft :: (Tenkei a, Tenkei b) => a -> b -> a
chooseLeft arg1 arg2 = unsafePerformIO $ do
  let arg1' = arg1
  let arg2' = arg2
  return $ deserialize $ callCBOR foreign_tenkei_choose_left tenkei_free $ CBOR_Array [serialize arg1', serialize arg2']

foreign import ccall "tenkei_reverse_list" foreign_tenkei_reverse_list :: Ptr Word8 -> CSize -> Ptr (Ptr Word8) -> Ptr CSize -> IO ()
reverseList :: (Tenkei a) => [a] -> [a]
reverseList arg1 = unsafePerformIO $ do
  let arg1' = arg1
  return $ deserialize $ callCBOR foreign_tenkei_reverse_list tenkei_free $ CBOR_Array [serialize arg1']

