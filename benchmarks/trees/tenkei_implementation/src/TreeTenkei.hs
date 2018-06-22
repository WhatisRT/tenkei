{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TreeTenkei where

import Foreign
import Foreign.C

import Data.CBOR
import FFIWrappers
import Tenkei
import CBOR
import Tree
import Data.Reflection
import Data.Proxy
import Unsafe.Coerce
import System.IO.Unsafe

tenkei_free :: Ptr Word8 -> CSize -> IO ()
tenkei_free = tenkeiFree
foreign export ccall tenkei_free :: Ptr Word8 -> CSize -> IO ()

tenkei_flatten_helper :: (Tree TenkeiValue) -> [TenkeiValue]
tenkei_flatten_helper = flatten
tenkei_flatten :: Ptr Word8 -> CSize -> Ptr (Ptr Word8) -> Ptr CSize -> IO ()
tenkei_flatten cbor = unsafePerformIO $ do
  -- print cbor
  return $ offerCBOR (\(CBOR_Array [arg1]) -> serialize $ tenkei_flatten_helper (deserialize arg1)) cbor
foreign export ccall tenkei_flatten :: Ptr Word8 -> CSize -> Ptr (Ptr Word8) -> Ptr CSize -> IO ()

newtype Ordable s a = Ordable { unordable :: a }

instance Reifies s (a -> a -> Ordering) => Eq (Ordable s a) where
  Ordable a == Ordable b = reflect (Proxy :: Proxy s) a b == EQ

instance Reifies s (a -> a -> Ordering) => Ord (Ordable s a) where
  compare (Ordable a) (Ordable b) = reflect (Proxy :: Proxy s) a b

ord_helper :: forall a x y. (a -> a -> Ordering) -> (forall b. Ord b => x b -> y b) -> x a -> y a
ord_helper f g xs = reify f $ \(Proxy :: Proxy s) -> unsafeCoerce (g (unsafeCoerce xs :: x (Ordable s a)))

tenkei_find_minimum_helper :: (TenkeiValue -> TenkeiValue -> Ordering) -> Tree TenkeiValue -> Maybe TenkeiValue
tenkei_find_minimum_helper f =
  ord_helper f findMinimum

tenkei_find_minimum :: Ptr Word8 -> CSize -> Ptr (Ptr Word8) -> Ptr CSize -> IO ()
tenkei_find_minimum cbor =
  offerCBOR (\(CBOR_Array [arg1, arg2]) -> serialize $ tenkei_find_minimum_helper (deserialize arg1) (deserialize arg2)) cbor
foreign export ccall tenkei_find_minimum :: Ptr Word8 -> CSize -> Ptr (Ptr Word8) -> Ptr CSize -> IO ()
