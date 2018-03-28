{-# LANGUAGE ForeignFunctionInterface #-}

module TestLibraryTenkei where

import Foreign
import Foreign.C

import Data.CBOR
import FFIWrappers
import Tenkei
import TestLibrary

tenkei_free :: Ptr Word8 -> CSize -> IO ()
tenkei_free = tenkeiFree
foreign export ccall tenkei_free :: Ptr Word8 -> CSize -> IO ()

tenkei_modify_array :: Ptr Word8 -> CSize -> Ptr (Ptr Word8) -> Ptr CSize -> IO ()
tenkei_modify_array = offer modifyArray
foreign export ccall tenkei_modify_array :: Ptr Word8 -> CSize -> Ptr (Ptr Word8) -> Ptr CSize -> IO ()

tenkei_invert_string_case :: Ptr Word8 -> CSize -> Ptr (Ptr Word8) -> Ptr CSize -> IO ()
tenkei_invert_string_case = offer invertStringCase
foreign export ccall tenkei_invert_string_case :: Ptr Word8 -> CSize -> Ptr (Ptr Word8) -> Ptr CSize -> IO ()

tenkei_exponentiate :: Ptr Word8 -> CSize -> Ptr (Ptr Word8) -> Ptr CSize -> IO ()
tenkei_exponentiate = offerCBOR (\(CBOR_Array [arg1, arg2]) -> serialize $ exponentiate (deserialize arg1) (deserialize arg2))
foreign export ccall tenkei_exponentiate :: Ptr Word8 -> CSize -> Ptr (Ptr Word8) -> Ptr CSize -> IO ()
