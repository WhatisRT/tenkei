{-# LANGUAGE ExistentialQuantification #-}

module FFIWrappers
  ( tenkeiFree
  , callForeign
  , offerForeign
  , tenkeiFreePtr
  , TenkeiPtr (..)
  , TenkeiFunPtr (..)
  ) where

import Foreign
import Foreign.C
import System.IO.Unsafe

newtype TenkeiPtr = TenkeiPtr { getPtr :: Ptr ()}

data TenkeiFunPtr = TenkeiFunPtr { funPtr :: TenkeiPtr, freePtr :: TenkeiPtr, dataPtr :: TenkeiPtr }

foreign import ccall "wrapper" freeFunctionToPtr :: (Ptr Word8 -> CSize -> IO ()) -> IO (FunPtr (Ptr Word8 -> CSize -> IO ()))

tenkeiFree :: Ptr Word8 -> CSize -> IO ()
tenkeiFree args _ = free args

tenkeiFreePtr :: FunPtr (Ptr Word8 -> CSize -> IO ())
tenkeiFreePtr = unsafePerformIO $ freeFunctionToPtr tenkeiFree

callForeign :: (Ptr Word8 -> CSize -> Ptr (Ptr Word8) -> Ptr CSize -> IO ()) -> (Ptr Word8 -> CSize -> IO ()) -> [Word8] -> IO [Word8]
callForeign function freeFunction bytes =
  withArray bytes $ \ptr ->
    (alloca
       (\res_ptr ->
          alloca
            (\res_size -> do
               function ptr (fromIntegral $ length bytes) res_ptr res_size
               res_ptr' <- peek res_ptr
               res_size' <- peek res_size
               res <- peekArray (fromEnum res_size') res_ptr'
               freeFunction res_ptr' res_size'
               return res)))

offerForeign :: ([Word8] -> [Word8]) -> Ptr Word8 -> CSize -> Ptr (Ptr Word8) -> Ptr CSize -> IO ()
offerForeign f args argn res resn = do
  bytes <- peekArray (fromEnum argn) args
  let x = f bytes
  poke resn $ fromIntegral $ length x
  res_ptr <- newArray x
  poke res res_ptr
