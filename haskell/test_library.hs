{-# LANGUAGE ForeignFunctionInterface #-}

module Test where

import Foreign
import Foreign.C
import Foreign.Ptr
import Foreign.Marshal.Alloc

import Data.CBOR
import Data.Binary.CBOR

import Data.ByteString.Lazy (ByteString, unpack, pack)

import Data.Binary.Put
import Data.Binary.Get

import Data.Maybe

foreign export ccall tenkei_free :: Ptr Word8 -> CSize -> IO ()
foreign export ccall quadruple :: Ptr Word8 -> CSize -> Ptr (Ptr Word8) -> Ptr CSize -> IO ()

tenkei_free :: Ptr Word8 -> CSize -> IO ()
tenkei_free args size = free args

offerFunction :: (Int32 -> Int32) -> Ptr Word8 -> CSize -> Ptr (Ptr Word8) -> Ptr CSize -> IO ()
offerFunction f args argn res resn = do 
                                x <- fmap (serializeInt . fromIntegral . f . fromJust . cborToInt) $ fromC args argn
                                poke resn $ fromIntegral $ length x
                                res_ptr <- newArray x
                                poke res $ res_ptr

quadruple = offerFunction quadrupleImpl

quadrupleImpl x = 4 * x
                                    
fromC :: Ptr Word8 -> CSize -> IO CBOR
fromC args argn = fmap toCBOR $ peekArray (fromEnum argn) args

toCBOR :: [Word8] -> CBOR
toCBOR = runGet getCBOR . pack

cborToInt :: CBOR -> Maybe Integer
cborToInt (CBOR_UInt i) = Just i
cborToInt (CBOR_SInt i) = Just i
cborToInt _ = Nothing

serializeInt :: Int32 -> [Word8]
serializeInt = unpack . runPut . putCBOR . intToCBOR

intToCBOR :: Int32 -> CBOR
intToCBOR i | i >= 0 = CBOR_UInt $ fromIntegral i
            | otherwise = CBOR_SInt $ fromIntegral i