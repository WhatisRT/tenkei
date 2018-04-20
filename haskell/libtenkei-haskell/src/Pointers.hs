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

newtype TenkeiValue = TenkeiValue { getValue :: CBOR }

instance Tenkei TenkeiValue where
  serialize = getValue
  deserialize = TenkeiValue

data TenkeiFunPtr = TenkeiFunPtr { funPtr :: TenkeiPtr, freePtr :: TenkeiPtr, dataPtr :: TenkeiPtr }

instance Tenkei TenkeiFunPtr where
  serialize p = CBOR_Array $ fmap serialize [funPtr p, freePtr p, dataPtr p]
  deserialize (CBOR_Array [fct, fr, d]) = TenkeiFunPtr (deserialize fct) (deserialize fr) (deserialize d)
  deserialize x = error ("Error while interpreting CBOR: not a function:\n" ++ show x)
