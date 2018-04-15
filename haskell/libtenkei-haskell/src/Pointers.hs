{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Pointers where

import Foreign
import Data.CBOR
import Tenkei

newtype TenkeiPtr = TenkeiPtr { getPtr :: Ptr ()}

instance Tenkei TenkeiPtr where
  serialize = CBOR_UInt . fromIntegral . (\(WordPtr x) -> x) . ptrToWordPtr . getPtr
  deserialize (CBOR_UInt i) = TenkeiPtr $ wordPtrToPtr $ WordPtr $ fromIntegral i
  deserialize x = error ("Error while interpreting CBOR: not a memory address:\n" ++ show x)
