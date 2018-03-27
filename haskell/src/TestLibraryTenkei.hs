{-# LANGUAGE ForeignFunctionInterface #-}

module TestLibraryTenkei where

import Foreign
import Foreign.C

import FFIWrappers
import TestLibrary

foreign export ccall tenkei_free :: Ptr Word8 -> CSize -> IO ()
foreign export ccall quadruple :: Ptr Word8 -> CSize -> Ptr (Ptr Word8) -> Ptr CSize -> IO ()

tenkei_free :: Ptr Word8 -> CSize -> IO ()
tenkei_free = tenkeiFree

quadruple :: Ptr Word8 -> CSize -> Ptr (Ptr Word8) -> Ptr CSize -> IO ()
quadruple = offer quadrupleImpl
