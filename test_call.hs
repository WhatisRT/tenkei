{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP                      #-}

import Foreign
import Foreign.C
import Foreign.Ptr

import Data.CBOR
import Data.Binary.CBOR

import Data.ByteString.Lazy (ByteString, unpack, pack)

import Data.Binary.Put
import Data.Binary.Get

import Data.Maybe

-- #include "rust/src/tenkei_rust.h"

foreign import ccall "triple" c_triple :: Ptr Word8 -> CSize -> Ptr (Ptr Word8) -> Ptr CSize -> IO ()
	
serializeInt :: Integer -> [Word8]
serializeInt = unpack . runPut . putCBOR . intToCBOR

intToCBOR :: Integer -> CBOR
intToCBOR i | i >= 0 = CBOR_UInt i
			| otherwise = CBOR_SInt i

deserializeInt :: [Word8] -> Integer
deserializeInt i = fromJust $ cborToInt $ runGet getCBOR $ pack i

cborToInt :: CBOR -> Maybe Integer
cborToInt (CBOR_UInt i) = Just i
cborToInt (CBOR_SInt i) = Just i
cborToInt _ = Nothing

f = (fmap deserializeInt) . (call c_triple) . serializeInt

call :: (Ptr Word8 -> CSize -> Ptr (Ptr Word8) -> Ptr CSize -> IO ()) -> [Word8] -> IO [Word8]
call function bytes = withArray bytes $ \ptr -> (alloca (\res_ptr -> alloca (\res_size ->
					do
						function ptr (fromIntegral (length bytes)) res_ptr res_size
						res_ptr' <- peek res_ptr
						res_size' <- peek res_size
						peekArray (fromEnum res_size') res_ptr'
					)))