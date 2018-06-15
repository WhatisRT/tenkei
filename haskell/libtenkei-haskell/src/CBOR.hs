module CBOR
  ( callCBOR
  , callCBORIO
  , offerCBOR
  , toFunPointer
  , fromFunPointer
  ) where

import FFIWrappers

import Foreign
import Foreign.C
import System.IO.Unsafe

import Data.Binary.CBOR
import Data.CBOR

import Data.Binary.Get
import Data.Binary.Put

import Data.ByteString.Lazy (pack, unpack)

foreign import ccall "dynamic" cborPtrToFunction
  :: FunPtr
  (Ptr () -> Ptr Word8 -> CSize -> Ptr (Ptr Word8) -> Ptr CSize -> IO ())
  -> (Ptr () -> Ptr Word8 -> CSize -> Ptr (Ptr Word8) -> Ptr CSize -> IO ())

foreign import ccall "wrapper" cborFunctionPtrToPtr
  :: (CSize -> Ptr Word8 -> CSize -> Ptr (Ptr Word8) -> Ptr CSize -> IO ()) ->
  IO
    (FunPtr
       (CSize -> Ptr Word8 -> CSize -> Ptr (Ptr Word8) -> Ptr CSize -> IO ()))

foreign import ccall "wrapper" cborFunctionToPtr
  :: (Ptr Word8 -> CSize -> Ptr (Ptr Word8) -> Ptr CSize -> IO ()) ->
  IO
    (FunPtr
       (Ptr Word8 -> CSize -> Ptr (Ptr Word8) -> Ptr CSize -> IO ()))

cborToBinary :: CBOR -> [Word8]
cborToBinary = unpack . runPut . putCBOR

binaryToCBOR :: [Word8] -> CBOR
binaryToCBOR = runGet getCBOR . pack

callCBORIO :: (Ptr Word8 -> CSize -> Ptr (Ptr Word8) -> Ptr CSize -> IO ()) -> (Ptr Word8 -> CSize -> IO ()) -> CBOR -> IO CBOR
callCBORIO function freeFunction = fmap binaryToCBOR . (callForeign function freeFunction) . cborToBinary

callCBOR :: (Ptr Word8 -> CSize -> Ptr (Ptr Word8) -> Ptr CSize -> IO ()) -> (Ptr Word8 -> CSize -> IO ()) -> CBOR -> CBOR
callCBOR function freeFunction input = unsafePerformIO $ callCBORIO function freeFunction input

offerCBOR :: (CBOR -> CBOR) -> Ptr Word8 -> CSize -> Ptr (Ptr Word8) -> Ptr CSize -> IO ()
offerCBOR f = offerForeign (cborToBinary . f . binaryToCBOR)

-- Throw away the data for function pointers
offerCBORPtr :: (CBOR -> CBOR) -> CSize -> Ptr Word8 -> CSize -> Ptr (Ptr Word8) -> Ptr CSize -> IO ()
offerCBORPtr f d = offerCBOR f

callCBORPtr :: Ptr () -> (Ptr () -> Ptr Word8 -> CSize -> Ptr (Ptr Word8) -> Ptr CSize -> IO ()) -> (Ptr Word8 -> CSize -> IO ()) -> CBOR -> CBOR
callCBORPtr d f = callCBOR (f d)

toPointer :: a -> IO TenkeiPtr
toPointer = fmap (TenkeiPtr . castStablePtrToPtr) . newStablePtr

toFunPointer :: (CBOR -> CBOR) -> IO TenkeiFunPtr
toFunPointer = (fmap toTenkeiFunPtr) . cborFunctionPtrToPtr . offerCBORPtr
  where
    toTenkeiFunPtr :: FunPtr a -> TenkeiFunPtr
    toTenkeiFunPtr f = TenkeiFunPtr (TenkeiPtr $ castFunPtrToPtr f) (TenkeiPtr $ castFunPtrToPtr tenkeiFreePtr) $ TenkeiPtr nullPtr

fromPointer :: TenkeiPtr -> IO a
fromPointer x = do
  let stable = castPtrToStablePtr $ getPtr x
  contents <- deRefStablePtr stable
  freeStablePtr stable
  return contents

fromFunPointer :: TenkeiFunPtr -> CBOR -> CBOR
fromFunPointer f = callCBORPtr (getPtr $ dataPtr f) (cborPtrToFunction $ castPtrToFunPtr $ getPtr $ funPtr f) (\_ _ -> return ()) -- this leaks memory! Replace by freePtr later!

toPointerF :: (Traversable f) => f a -> IO (f TenkeiPtr)
toPointerF = traverse toPointer
