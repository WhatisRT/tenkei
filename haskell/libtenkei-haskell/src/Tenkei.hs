{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}

module Tenkei where

import Data.CBOR
import Data.Maybe
import Generics.SOP

import Data.Int

class Tenkei a where
  serialize :: a -> CBOR
  default serialize :: (Generic a, All2 Tenkei (Code a)) =>
    a -> CBOR
  serialize = serializeS 0 . from
  deserialize :: CBOR -> a
  default deserialize :: (Generic a, All2 Tenkei (Code a)) =>
    CBOR -> a
  deserialize = to . deserializeS

instance Tenkei Int32 where
  serialize i
    | i >= 0 = CBOR_UInt $ fromIntegral i
    | otherwise = CBOR_SInt $ fromIntegral i
  deserialize (CBOR_UInt i) = fromIntegral i
  deserialize (CBOR_SInt i) = fromIntegral i
  deserialize x = error ("Error while interpreting CBOR: not an integer:\n" ++ show x)

instance Tenkei Char where
  serialize = CBOR_UInt . fromIntegral . fromEnum
  deserialize (CBOR_UInt b) = toEnum $ fromIntegral b
  deserialize x = error ("Error while interpreting CBOR: not a character:\n" ++ show x)

instance Tenkei a => Tenkei [a] where
  serialize = CBOR_Array . fmap serialize
  deserialize (CBOR_Array a) = fmap deserialize a
  deserialize x = error ("Error while interpreting CBOR: not an array:\n" ++ show x)

serializeS :: All2 Tenkei xss => Integer -> SOP I xss -> CBOR
serializeS layer (SOP (Z xs)) = CBOR_Array [CBOR_UInt layer, CBOR_Array $ serializeP xs]
serializeS layer (SOP (S xss)) = serializeS (layer + 1) $ SOP xss

serializeP :: (All Tenkei xs) => NP I xs -> [CBOR]
serializeP Nil = []
serializeP (I x :* xss) = serialize x : serializeP xss

deserializeS :: (All2 Tenkei xss, SListI xss) => CBOR -> SOP I xss
deserializeS (CBOR_Array [CBOR_UInt n, CBOR_Array x]) = SOP $ helper2 $ catMaybes $ hcollapse $ sumContents injections countList
  where
    allTC = Proxy :: Proxy (All Tenkei)
    sumContents :: (All2 Tenkei xss) => NP (Injection (NP I) xss) xss -> NP (K Integer) xss -> NP (K (Maybe ((NS (NP I)) xss))) xss
    sumContents = hczipWith allTC helper
    helper :: (All Tenkei xs) => Injection (NP I) xss xs -> K Integer xs -> K (Maybe (NS (NP I) xss)) xs
    helper (Fn i) (K k)
      | k == n = K $ Just $ unK $ i $ deserializeP x
      | otherwise = K Nothing
    helper2 :: [a] -> a
    helper2 [y] = y
    helper2 _ = error "Error while interpreting CBOR in sum type"
deserializeS _ = error "Error while interpreting CBOR in sum type"

countList ::
     forall xss. SListI xss
  => NP (K Integer) xss
countList =
  case sList :: SList xss of
    SNil -> Nil
    SCons -> K 0 :* hmap (\(K i) -> K $ i + 1) countList

deserializeP ::
     forall xs. (SListI xs, All Tenkei xs)
  => [CBOR]
  -> NP I xs
deserializeP [] =
  case sList :: SList xs of
    SNil -> Nil
    SCons -> error "Error while interpreting CBOR in product type"
deserializeP (y:ys) =
  case sList :: SList xs of
    SNil -> error "Error while interpreting CBOR in product type"
    SCons -> I (deserialize y) :* deserializeP ys
