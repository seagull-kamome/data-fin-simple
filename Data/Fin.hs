{-# LANGUAGE KindSignatures,
             DataKinds,
             StandaloneDeriving,
             TypeOperators,
             TypeFamilies,
             DeriveDataTypeable,
             RankNTypes,
             ScopedTypeVariables,
             FlexibleContexts,
             FlexibleInstances #-}
module Data.Fin (
  Fin (..),
  natToFin, toFin, unsafeToFin, fromFin,
  finZ, finS, finLast,
  absurd,
  weaken, weakenN, strengthen,
  shift,
  finAdd, finAddN,
  finSub, finSubN,
  finMult
  ) where

import GHC.TypeLits
import Data.Proxy
import Data.Typeable
import Unsafe.Coerce

newtype Fin s (n :: Nat) = Fin s
                         deriving (Typeable)

deriving instance Ord s => Ord (Fin s n)
deriving instance Eq s => Eq (Fin s n)
deriving instance Show s => Show (Fin s n)

-- instance (Integral s, KnownNat (n + 1)) => Enum (Fin s (n + 1)) where
--   succ = finS
--   pred x@(Fin x')
--     | x == 0 = x
--     | otherwise = Fin (x - 1)
--  toEnum x = toFin . fromIntegral
--  fromEnum (Fin x) = fromIntegral x

finZ :: (Integral s, KnownNat (n + 1)) => Fin s (n + 1)
finZ = Fin 0
{-# INLINE finZ #-}

finS :: (Integral s, KnownNat n, KnownNat (n + 1)) => Fin s n -> Fin s (n + 1)
finS (Fin x) = Fin x
{-# INLINE finS #-}

-- |
-- >>> (finLast :: Fin Int 5)
-- Fin 4
--
finLast :: forall s n. (Integral s, KnownNat n, KnownNat (n + 1)) => Fin s (n + 1)
finLast = unsafeToFin $ fromIntegral $ natVal (Proxy :: Proxy n)
{-# INLINE finLast #-}

absurd :: Fin s 0 -> a
absurd = unsafeCoerce
{-# INLINE absurd #-}

-- |
-- >>> (natToFin (Proxy :: Proxy 10) :: Fin Int 11)
-- Fin 10
--
natToFin :: (Integral s, KnownNat n, KnownNat (m + 1), n <= m) => proxy n -> Fin s (m + 1)
natToFin = unsafeToFin . fromIntegral . natVal
{-# INLINE natToFin #-}


-- |
-- >>> (toFin 0 :: Maybe (Fin Int 4))
-- Just (Fin 0)
--
-- >>> (toFin 4 :: Maybe (Fin Int 4))
-- Nothing
--
toFin :: forall s n. (Integral s, KnownNat (n :: Nat)) => s -> Maybe (Fin s n)
toFin x
  | x < 0 = Nothing
  | x < fromIntegral (natVal (Proxy :: Proxy n)) = Just $ Fin x
  | otherwise = Nothing

-- |
-- >>> (unsafeToFin 10 :: Fin Int 5)
-- Fin 10
--
unsafeToFin :: (Integral s, KnownNat n) => s -> Fin s n
unsafeToFin = Fin
{-# INLINE unsafeToFin #-}


-- |
-- >>> fromFin (finZ :: Fin Int 10)
-- 0
--
-- >>> fromFin (finLast :: Fin Int 10)
-- 9
--
fromFin :: Fin s n -> s
fromFin (Fin x) = x
{-# INLINE fromFin #-}


weaken :: (Integral s, KnownNat (n + 1)) => Fin s n -> Fin s (n + 1)
weaken (Fin x) = unsafeToFin x
{-# INLINE weaken #-}

weakenN :: (Integral s, KnownNat (n + m)) => proxy m -> Fin s n -> Fin s (n + m)
weakenN _ (Fin x) = unsafeToFin x
{-# INLINE weakenN #-}

strengthen :: forall n s. (KnownNat n, Integral s, Ord s) => Fin s (n + 1) -> Either (Fin s (n + 1)) (Fin s n)
strengthen x'@(Fin x)
  | x < fromIntegral (natVal (Proxy :: Proxy n)) = Right $ unsafeToFin x
  | otherwise = Left x'
{-# INLINE strengthen #-}

-- |
-- >>> shift (Proxy :: Proxy 1) (finZ :: Fin Int 20)
-- Fin 0
--
-- >>> (shift (Proxy :: Proxy 10) (finLast :: Fin Int 10) :: Fin Int 20)
-- Fin 9
--
shift :: (KnownNat (m + n), Integral s) => proxy m -> Fin s n -> Fin s (m + n)
shift _ (Fin x) = unsafeToFin x
{-# INLINE shift #-}

finAdd :: (Integral s, KnownNat (n + m)) => Fin s n -> Fin s m -> Fin s (n + m)
finAdd (Fin x) (Fin y) = unsafeToFin $ x + y
{-# INLINE finAdd #-}

finAddN :: forall n s. (KnownNat n, Integral s, Ord s) => Fin s n -> s -> Either (Fin s n) (Fin s n)
finAddN x@(Fin x') y
  | fromIntegral (natVal (Proxy :: Proxy n)) - x' > y = Right $ Fin (x' + y)
  | otherwise = Left x
{-# INLINE finAddN #-}

finSub :: (KnownNat n, Ord s, Integral s) => Fin s n -> Fin s m -> Either (Fin s n) (Fin s n)
finSub x (Fin y) = finSubN x y
{-# INLINE finSub #-}

finSubN :: (KnownNat n, Ord s, Integral s) => Fin s n -> s -> Either (Fin s n) (Fin s n)
finSubN x'@(Fin x) y
  | y <= x = Right $ unsafeToFin $ x - y
  | otherwise = Left $ x'
{-# INLINE finSubN #-}


finMult :: (KnownNat (n * m), Integral s) => Fin s n -> Fin s m -> Fin s (n * m)
finMult (Fin x) (Fin y) = unsafeToFin $ x * y
{-# INLINE finMult #-}

