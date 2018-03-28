module FFIWrappers
  ( call
  , offer
  , tenkeiFree
  ) where

import Foreign
import Foreign.C
import System.IO.Unsafe

import Data.Binary.CBOR
import Data.CBOR

import Data.Binary.Get
import Data.Binary.Put

import Data.ByteString.Lazy (pack, unpack)

import Tenkei

callIO :: (Tenkei a, Tenkei b) => (Ptr Word8 -> CSize -> Ptr (Ptr Word8) -> Ptr CSize -> IO ()) -> (Ptr Word8 -> CSize -> IO ()) -> a -> IO b
callIO function freeFunction input =
  let bytes = cborToBinary $ serialize input
   in withArray bytes $ \ptr ->
        (alloca
           (\res_ptr ->
              alloca
                (\res_size -> do
                   function ptr (fromIntegral (length bytes)) res_ptr res_size
                   res_ptr' <- peek res_ptr
                   res_size' <- peek res_size
                   res <- peekArray (fromEnum res_size') res_ptr'
                   freeFunction res_ptr' res_size'
                   return $ deserialize $ binaryToCBOR res)))

call :: (Tenkei a, Tenkei b) => (Ptr Word8 -> CSize -> Ptr (Ptr Word8) -> Ptr CSize -> IO ()) -> (Ptr Word8 -> CSize -> IO ()) -> a -> b
call f freeFunction = unsafePerformIO . callIO f freeFunction

offer :: (Tenkei a, Tenkei b) => (a -> b) -> Ptr Word8 -> CSize -> Ptr (Ptr Word8) -> Ptr CSize -> IO ()
offer f args argn res resn = do
  bytes <- peekArray (fromEnum argn) args
  let x = cborToBinary $ serialize $ f $ deserialize $ binaryToCBOR bytes
  poke resn $ fromIntegral $ length x
  res_ptr <- newArray x
  poke res res_ptr

tenkeiFree :: Ptr Word8 -> CSize -> IO ()
tenkeiFree args _ = free args

cborToBinary :: CBOR -> [Word8]
cborToBinary = unpack . runPut . putCBOR

binaryToCBOR :: [Word8] -> CBOR
binaryToCBOR = runGet getCBOR . pack