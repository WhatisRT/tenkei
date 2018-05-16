{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE DeriveGeneric #-}

module TreeTenkei where

import Foreign
import Foreign.C
import Data.CBOR
import FFIWrappers
import System.IO.Unsafe
import Generics.SOP hiding (Nil)
import Tenkei
import qualified GHC.Generics as GHC

data Tree a = Nil | Node a (Tree a) (Tree a) deriving (GHC.Generic)

instance Generic (Tree a)

instance Tenkei a => Tenkei (Tree a)

callCBORDebug :: (Ptr Word8 -> CSize -> Ptr (Ptr Word8) -> Ptr CSize -> IO ()) -> (Ptr Word8 -> CSize -> IO ()) -> CBOR -> CBOR
callCBORDebug f g cbor = unsafePerformIO $ do
  print cbor
  return $ callCBOR f g cbor

foreign import ccall "tenkei_free" tenkei_free :: Ptr Word8 -> CSize -> IO ()

foreign import ccall "tenkei_flatten" foreign_tenkei_flatten :: Ptr Word8 -> CSize -> Ptr (Ptr Word8) -> Ptr CSize -> IO ()
flatten :: (Tenkei a) => (Tree a) -> [a]
flatten arg1 = unsafePerformIO $ do
  let arg1' = arg1
  return $ deserialize $ callCBOR foreign_tenkei_flatten tenkei_free $ CBOR_Array [serialize arg1']

foreign import ccall "tenkei_find_minimum" foreign_tenkei_find_minimum :: Ptr Word8 -> CSize -> Ptr (Ptr Word8) -> Ptr CSize -> IO ()
findMinimum :: (Tree Int32) -> (Maybe Int32)
findMinimum arg1 = unsafePerformIO $ do
  let arg1' = arg1
  return $ deserialize $ callCBOR foreign_tenkei_find_minimum tenkei_free $ CBOR_Array [serialize arg1']

