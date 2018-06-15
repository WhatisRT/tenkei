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
import FFIWrappers
import CBOR

import Data.ByteString.Conversion

import Data.Int
import Foreign
import Foreign.C
import System.IO.Unsafe

instance Tenkei TenkeiPtr where
  serialize = CBOR_UInt . fromIntegral . (\(WordPtr x) -> x) . ptrToWordPtr . getPtr
  deserialize (CBOR_UInt i) = TenkeiPtr $ wordPtrToPtr $ WordPtr $ fromIntegral i
  deserialize x = error ("Error while interpreting CBOR: not a memory address:\n" ++ show x)

newtype TenkeiValue = TenkeiValue { getValue :: CBOR }

instance Tenkei TenkeiValue where
  serialize = getValue
  deserialize = TenkeiValue

instance Tenkei TenkeiFunPtr where
  serialize p = CBOR_Array $ fmap serialize [funPtr p, freePtr p, dataPtr p]
  deserialize (CBOR_Array [fct, fr, d]) = TenkeiFunPtr (deserialize fct) (deserialize fr) (deserialize d)
  deserialize x = error ("Error while interpreting CBOR: not a function:\n" ++ show x)

callIO ::
     (Tenkei a, Tenkei b) => (Ptr Word8 -> CSize -> Ptr (Ptr Word8) -> Ptr CSize -> IO ()) -> (Ptr Word8 -> CSize -> IO ()) -> a -> IO b
callIO function freeFunction input = fmap deserialize $ callCBORIO function freeFunction $ serialize input

call :: (Tenkei a, Tenkei b) => (Ptr Word8 -> CSize -> Ptr (Ptr Word8) -> Ptr CSize -> IO ()) -> (Ptr Word8 -> CSize -> IO ()) -> a -> b
call f freeFunction = unsafePerformIO . callIO f freeFunction

offer :: (Tenkei a, Tenkei b) => (a -> b) -> Ptr Word8 -> CSize -> Ptr (Ptr Word8) -> Ptr CSize -> IO ()
offer f = offerCBOR $ serialize . f . deserialize

class Tenkei a where
  serialize :: a -> CBOR
  default serialize :: (Generic a, All2 Tenkei (Code a)) =>
    a -> CBOR
  serialize = serializeS 0 . from
  deserialize :: CBOR -> a
  default deserialize :: (Generic a, All2 Tenkei (Code a)) =>
    CBOR -> a
  deserialize = to . deserializeS

instance (Tenkei a, Tenkei b) => Tenkei (a -> b) where
  serialize f = serialize $ unsafePerformIO $ toFunPointer $ serialize . f . (\(CBOR_Array [x]) -> deserialize x)
  deserialize i = deserialize . fromFunPointer (deserialize i) . serialize

instance Tenkei Bool where
  serialize = CBOR_UInt . fromIntegral . fromEnum
  deserialize (CBOR_UInt i) = i == 1
  deserialize _ = error "Error while interpreting CBOR: not a boolean"

instance Tenkei Int8 where
  serialize i
    | i >= 0 = CBOR_UInt $ fromIntegral i
    | otherwise = CBOR_SInt $ fromIntegral i
  deserialize (CBOR_UInt i) = fromIntegral i
  deserialize (CBOR_SInt i) = fromIntegral i
  deserialize x = error ("Error while interpreting CBOR: not an integer:\n" ++ show x)

instance Tenkei Int16 where
  serialize i
    | i >= 0 = CBOR_UInt $ fromIntegral i
    | otherwise = CBOR_SInt $ fromIntegral i
  deserialize (CBOR_UInt i) = fromIntegral i
  deserialize (CBOR_SInt i) = fromIntegral i
  deserialize x = error ("Error while interpreting CBOR: not an integer:\n" ++ show x)

instance Tenkei Int32 where
  serialize i
    | i >= 0 = CBOR_UInt $ fromIntegral i
    | otherwise = CBOR_SInt $ fromIntegral i
  deserialize (CBOR_UInt i) = fromIntegral i
  deserialize (CBOR_SInt i) = fromIntegral i
  deserialize x = error ("Error while interpreting CBOR: not an integer:\n" ++ show x)

instance Tenkei Int64 where
  serialize i
    | i >= 0 = CBOR_UInt $ fromIntegral i
    | otherwise = CBOR_SInt $ fromIntegral i
  deserialize (CBOR_UInt i) = fromIntegral i
  deserialize (CBOR_SInt i) = fromIntegral i
  deserialize x = error ("Error while interpreting CBOR: not an integer:\n" ++ show x)

instance Tenkei Float where
  serialize = CBOR_Float
  deserialize (CBOR_Float x) = x
  deserialize x = error ("Error while interpreting CBOR: not a float:\n" ++ show x)

instance Tenkei Double where
  serialize = CBOR_Double
  deserialize (CBOR_Double x) = x
  deserialize x = error ("Error while interpreting CBOR: not a float:\n" ++ show x)

instance Tenkei Char where -- codepoint_unicode
  serialize = CBOR_UInt . fromIntegral . fromEnum
  deserialize (CBOR_UInt b) = toEnum $ fromIntegral b
  deserialize x = error ("Error while interpreting CBOR: not a character:\n" ++ show x)

instance Tenkei Integer where
  serialize = CBOR_BS . toByteString'
  deserialize (CBOR_BS s) | (Just i) <- fromByteString s = i
                          | otherwise = error "Error while interpreting CBOR: not a bigint"

instance Tenkei a => Tenkei [a] where
  serialize = CBOR_Array . fmap serialize
  deserialize (CBOR_Array a) = fmap deserialize a
  deserialize x = error ("Error while interpreting CBOR: not an array:\n" ++ show x)

instance Tenkei a => Tenkei (Maybe a)

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
