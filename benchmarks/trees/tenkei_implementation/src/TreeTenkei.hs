{-# LANGUAGE ForeignFunctionInterface #-}

module TreeTenkei where

import Foreign
import Foreign.C

import Data.CBOR
import FFIWrappers
import Pointers
import Tenkei
import Tree

import System.IO.Unsafe

tenkei_free :: Ptr Word8 -> CSize -> IO ()
tenkei_free = tenkeiFree
foreign export ccall tenkei_free :: Ptr Word8 -> CSize -> IO ()

tenkei_flatten_helper :: (Tree TenkeiValue) -> [TenkeiValue]
tenkei_flatten_helper = flatten
tenkei_flatten :: Ptr Word8 -> CSize -> Ptr (Ptr Word8) -> Ptr CSize -> IO ()
tenkei_flatten cbor = unsafePerformIO $ do
  -- print cbor
  return $ offerCBOR (\(CBOR_Array [arg1]) -> serialize $ tenkei_flatten_helper (deserialize arg1)) cbor
foreign export ccall tenkei_flatten :: Ptr Word8 -> CSize -> Ptr (Ptr Word8) -> Ptr CSize -> IO ()

tenkei_find_minimum :: Ptr Word8 -> CSize -> Ptr (Ptr Word8) -> Ptr CSize -> IO ()
tenkei_find_minimum cbor = unsafePerformIO $ do
  -- print cbor
  return $ offerCBOR (\(CBOR_Array [arg1]) -> serialize $ findMinimum (deserialize arg1)) cbor
foreign export ccall tenkei_find_minimum :: Ptr Word8 -> CSize -> Ptr (Ptr Word8) -> Ptr CSize -> IO ()

