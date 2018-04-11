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

tenkei_modify_array :: Ptr Word8 -> CSize -> Ptr (Ptr Word8) -> Ptr CSize -> IO ()
tenkei_modify_array = offerCBOR (\(CBOR_Array [arg1]) -> serialize $ modifyArray (deserialize arg1))
foreign export ccall tenkei_modify_array :: Ptr Word8 -> CSize -> Ptr (Ptr Word8) -> Ptr CSize -> IO ()

-- tenkei_invert_string_case :: Ptr Word8 -> CSize -> Ptr (Ptr Word8) -> Ptr CSize -> IO ()
-- tenkei_invert_string_case = offerCBOR (\(CBOR_Array [arg1]) -> serialize $ invertStringCase (deserialize arg1))
-- foreign export ccall tenkei_invert_string_case :: Ptr Word8 -> CSize -> Ptr (Ptr Word8) -> Ptr CSize -> IO ()

tenkei_exponentiate :: Ptr Word8 -> CSize -> Ptr (Ptr Word8) -> Ptr CSize -> IO ()
tenkei_exponentiate = offerCBOR (\(CBOR_Array [arg1, arg2]) -> serialize $ exponentiate (deserialize arg1) (deserialize arg2))
foreign export ccall tenkei_exponentiate :: Ptr Word8 -> CSize -> Ptr (Ptr Word8) -> Ptr CSize -> IO ()

tenkei_identity_helper :: TenkeiPtr -> TenkeiPtr
tenkei_identity_helper = identity
tenkei_identity :: Ptr Word8 -> CSize -> Ptr (Ptr Word8) -> Ptr CSize -> IO ()
tenkei_identity = offerCBOR (\(CBOR_Array [arg1]) -> serialize $ tenkei_identity_helper (deserialize arg1))
foreign export ccall tenkei_identity :: Ptr Word8 -> CSize -> Ptr (Ptr Word8) -> Ptr CSize -> IO ()

tenkei_choose_left_helper :: TenkeiPtr -> TenkeiPtr -> TenkeiPtr
tenkei_choose_left_helper = chooseLeft
tenkei_choose_left :: Ptr Word8 -> CSize -> Ptr (Ptr Word8) -> Ptr CSize -> IO ()
tenkei_choose_left = offerCBOR (\(CBOR_Array [arg1, arg2]) -> serialize $ tenkei_choose_left_helper (deserialize arg1) (deserialize arg2))
foreign export ccall tenkei_choose_left :: Ptr Word8 -> CSize -> Ptr (Ptr Word8) -> Ptr CSize -> IO ()

tenkei_reverse_list_helper :: [TenkeiPtr] -> [TenkeiPtr]
tenkei_reverse_list_helper = reverseList
tenkei_reverse_list :: Ptr Word8 -> CSize -> Ptr (Ptr Word8) -> Ptr CSize -> IO ()
tenkei_reverse_list = offerCBOR (\(CBOR_Array [arg1]) -> serialize $ tenkei_reverse_list_helper (deserialize arg1))
foreign export ccall tenkei_reverse_list :: Ptr Word8 -> CSize -> Ptr (Ptr Word8) -> Ptr CSize -> IO ()

tenkei_apply_function :: Ptr Word8 -> CSize -> Ptr (Ptr Word8) -> Ptr CSize -> IO ()
tenkei_apply_function = offerCBOR (\(CBOR_Array [arg1, arg2]) -> serialize $ applyFunction (fromFunPointer (deserialize arg1) tenkei_free) (deserialize arg2))
foreign export ccall tenkei_apply_function :: Ptr Word8 -> CSize -> Ptr (Ptr Word8) -> Ptr CSize -> IO ()

