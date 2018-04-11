{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Pointers (TenkeiPtr) where

import Foreign
import Data.CBOR
import Tenkei

type TenkeiPtr = Ptr ()

instance Tenkei TenkeiPtr where
  serialize = CBOR_UInt . fromIntegral . (\(WordPtr x) -> x) . ptrToWordPtr
  deserialize (CBOR_UInt i) = wordPtrToPtr $ WordPtr $ fromIntegral i
  deserialize x = error ("Error while interpreting CBOR: not a memory address:\n" ++ show x)
