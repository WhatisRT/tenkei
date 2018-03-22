{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}

module Tenkei where

import Foreign
import Foreign.C
import Foreign.Ptr

import Data.CBOR
import Data.Binary.CBOR

import Data.ByteString.Lazy (ByteString, unpack, pack)

import Data.Binary.Put
import Data.Binary.Get

import Data.Maybe

import GHC.Generics

call :: (Tenkei a, Tenkei b) => (Ptr Word8 -> CSize -> Ptr (Ptr Word8) -> Ptr CSize -> IO ()) -> (Ptr Word8 -> CSize -> IO ()) -> a -> IO b
call function free input | bytes <- cborToBinary $ serialize input = withArray bytes $ \ptr -> (alloca (\res_ptr -> alloca (\res_size ->
                    do
                        function ptr (fromIntegral (length bytes)) res_ptr res_size
                        res_ptr' <- peek res_ptr
                        res_size' <- peek res_size
                        res <- peekArray (fromEnum res_size') res_ptr'
                        free res_ptr' res_size'
                        return $ deserialize $ binaryToCBOR res
                    )))

cborToBinary :: CBOR -> [Word8]
cborToBinary = unpack . runPut . putCBOR

binaryToCBOR :: [Word8] -> CBOR
binaryToCBOR = runGet getCBOR . pack

class Tenkei a where
  serialize :: a -> CBOR
  default serialize :: (Generic a, Tenkei' (Rep a)) => a -> CBOR
  serialize = serialize' . from

  deserialize :: CBOR -> a
  default deserialize :: (Generic a, Tenkei' (Rep a)) => CBOR -> a
  deserialize = to . deserialize'

instance Tenkei Int32 where
  serialize i | i >= 0 = CBOR_UInt $ fromIntegral i
              | otherwise = CBOR_SInt $ fromIntegral i
  deserialize (CBOR_UInt i) = fromIntegral i
  deserialize (CBOR_SInt i) = fromIntegral i

data DummySum a = Dummy1 Int32 | Dummy2 Int32 | Dummy3 a deriving (Show,Generic)
data DummyProduct = Dummy Int32 (DummySum Int32) deriving (Show,Generic)
instance (Tenkei a) => Tenkei (DummySum a)
instance Tenkei DummyProduct

class Tenkei' a where
  serialize' :: a t -> CBOR
  deserialize' :: CBOR -> a t

instance Tenkei' V1 where
  serialize' x = undefined
  deserialize' x = undefined

instance Tenkei' U1 where
  serialize' U1 = CBOR_NULL
  deserialize' CBOR_NULL = U1

data EncancedCBOR = Sum CBOR | Product CBOR 

instance (Tenkei' f, Tenkei' g) => Tenkei' (f :+: g) where
  serialize' (L1 x) = CBOR_Array [CBOR_UInt 0, serialize' x]
  serialize' (R1 x) = CBOR_Array [CBOR_UInt 1, serialize' x]
  deserialize' (CBOR_Array [CBOR_UInt 0, cbor]) = L1 $ deserialize' cbor
  deserialize' (CBOR_Array [CBOR_UInt 1, cbor]) = R1 $ deserialize' cbor

instance (Tenkei' f, Tenkei' g) => Tenkei' (f :*: g) where
  serialize' (x :*: y) = CBOR_Array [serialize' x, serialize' y]
  deserialize' (CBOR_Array [x, y]) = (deserialize' x) :*: (deserialize' y)

instance (Tenkei c) => Tenkei' (K1 i c) where
  serialize' (K1 x) = serialize x
  deserialize' = K1 . deserialize

instance (Tenkei' f) => Tenkei' (M1 i t f) where
  serialize' (M1 x) = serialize' x
  deserialize' = M1 . deserialize'
