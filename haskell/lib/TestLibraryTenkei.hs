{-# LANGUAGE ForeignFunctionInterface #-}

module TestLibraryTenkei where

import Foreign
import Foreign.C

import Data.CBOR
import FFIWrappers
import Pointers
import Tenkei
import TestLibrary

tenkei_free :: Ptr Word8 -> CSize -> IO ()
tenkei_free = tenkeiFree
foreign export ccall tenkei_free :: Ptr Word8 -> CSize -> IO ()

tenkei_library_language :: Ptr Word8 -> CSize -> Ptr (Ptr Word8) -> Ptr CSize -> IO ()
tenkei_library_language = offerCBOR (\(CBOR_Array []) -> serialize $ libraryLanguage )
foreign export ccall tenkei_library_language :: Ptr Word8 -> CSize -> Ptr (Ptr Word8) -> Ptr CSize -> IO ()

tenkei_binary_or :: Ptr Word8 -> CSize -> Ptr (Ptr Word8) -> Ptr CSize -> IO ()
tenkei_binary_or = offerCBOR (\(CBOR_Array [arg1, arg2]) -> serialize $ binaryOr (deserialize arg1) (deserialize arg2))
foreign export ccall tenkei_binary_or :: Ptr Word8 -> CSize -> Ptr (Ptr Word8) -> Ptr CSize -> IO ()

tenkei_modify_array :: Ptr Word8 -> CSize -> Ptr (Ptr Word8) -> Ptr CSize -> IO ()
tenkei_modify_array = offerCBOR (\(CBOR_Array [arg1]) -> serialize $ modifyArray (deserialize arg1))
foreign export ccall tenkei_modify_array :: Ptr Word8 -> CSize -> Ptr (Ptr Word8) -> Ptr CSize -> IO ()

tenkei_exponentiate :: Ptr Word8 -> CSize -> Ptr (Ptr Word8) -> Ptr CSize -> IO ()
tenkei_exponentiate = offerCBOR (\(CBOR_Array [arg1, arg2]) -> serialize $ exponentiate (deserialize arg1) (deserialize arg2))
foreign export ccall tenkei_exponentiate :: Ptr Word8 -> CSize -> Ptr (Ptr Word8) -> Ptr CSize -> IO ()

tenkei_identity_helper :: TenkeiValue -> TenkeiValue
tenkei_identity_helper = identity
tenkei_identity :: Ptr Word8 -> CSize -> Ptr (Ptr Word8) -> Ptr CSize -> IO ()
tenkei_identity = offerCBOR (\(CBOR_Array [arg1]) -> serialize $ tenkei_identity_helper (deserialize arg1))
foreign export ccall tenkei_identity :: Ptr Word8 -> CSize -> Ptr (Ptr Word8) -> Ptr CSize -> IO ()

tenkei_choose_left_helper :: TenkeiValue -> TenkeiValue -> TenkeiValue
tenkei_choose_left_helper = chooseLeft
tenkei_choose_left :: Ptr Word8 -> CSize -> Ptr (Ptr Word8) -> Ptr CSize -> IO ()
tenkei_choose_left = offerCBOR (\(CBOR_Array [arg1, arg2]) -> serialize $ tenkei_choose_left_helper (deserialize arg1) (deserialize arg2))
foreign export ccall tenkei_choose_left :: Ptr Word8 -> CSize -> Ptr (Ptr Word8) -> Ptr CSize -> IO ()

tenkei_reverse_list_helper :: [TenkeiValue] -> [TenkeiValue]
tenkei_reverse_list_helper = reverseList
tenkei_reverse_list :: Ptr Word8 -> CSize -> Ptr (Ptr Word8) -> Ptr CSize -> IO ()
tenkei_reverse_list = offerCBOR (\(CBOR_Array [arg1]) -> serialize $ tenkei_reverse_list_helper (deserialize arg1))
foreign export ccall tenkei_reverse_list :: Ptr Word8 -> CSize -> Ptr (Ptr Word8) -> Ptr CSize -> IO ()

