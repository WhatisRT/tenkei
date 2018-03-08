{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP                      #-}

import Foreign
import Foreign.C
import Foreign.Ptr

import Data.CBOR
import Data.Binary.CBOR

#include "tenkei_rust.h"

foreign import ccall "triple" c_triple :: Ptr Word8 -> CSize -> Ptr (Ptr Word8) -> Ptr CSize -> IO ()
	
serializeInt :: Int -> [Word8]
serializeInt = unpack . runPut . putCBOR . CBOR_UInt

testCall :: [Word8] -> IO [Word8]
testCall bytes = withArray bytes $ \ptr ->
               		do
						let res_ptr = Ptr nullPtr
							res_size = Ptr 0
						in
							c_triple ptr (fromIntegral (length bytes)) res_ptr res_size
							res_size' <- peek res_size
                 	  		peekArray (fromEnum res_size') res_ptr