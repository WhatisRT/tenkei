{-# LANGUAGE ForeignFunctionInterface #-}

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

foreign import ccall "tenkei_free" c_tenkei_free :: Ptr Word8 -> CSize -> IO ()

call :: (Tenkei a, Tenkei b) => (Ptr Word8 -> CSize -> Ptr (Ptr Word8) -> Ptr CSize -> IO ()) -> a -> IO b
call function input | bytes <- serialize input = withArray bytes $ \ptr -> (alloca (\res_ptr -> alloca (\res_size ->
                    do
                        function ptr (fromIntegral (length bytes)) res_ptr res_size
                        res_ptr' <- peek res_ptr
                        res_size' <- peek res_size
                        res <- peekArray (fromEnum res_size') res_ptr'
                        c_tenkei_free res_ptr' res_size'
                        return $ deserialize res
                    )))

class Tenkei a where
  serialize :: a -> [Word8]
  deserialize :: [Word8] -> a

instance Tenkei Int32 where
  serialize = unpack . runPut . putCBOR . intToCBOR
  deserialize = fromJust . cborToInt . runGet getCBOR . pack

cborToInt :: CBOR -> Maybe Int32
cborToInt (CBOR_UInt i) = Just $ fromIntegral i
cborToInt (CBOR_SInt i) = Just $ fromIntegral i
cborToInt _ = Nothing

intToCBOR :: Int32 -> CBOR
intToCBOR i | i >= 0 = CBOR_UInt $ fromIntegral i
            | otherwise = CBOR_SInt $ fromIntegral i