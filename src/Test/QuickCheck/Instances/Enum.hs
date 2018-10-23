{-# language TypeApplications #-}

module Test.QuickCheck.Instances.Enum () where

import Data.Enum.Types
import Data.Coerce (coerce)
import Test.QuickCheck (Arbitrary(..), arbitraryBoundedEnum)

instance Arbitrary A where
  arbitrary = coerce (arbitrary @(Enumeration A))
  shrink = coerce (shrink @(Enumeration A))

instance Arbitrary B where
  arbitrary = coerce (arbitrary @(Enumeration B))
  shrink = coerce (shrink @(Enumeration B))

instance Arbitrary C where
  arbitrary = coerce (arbitrary @(Enumeration C))
  shrink = coerce (shrink @(Enumeration C))

instance Arbitrary D where
  arbitrary = coerce (arbitrary @(Enumeration D))
  shrink = coerce (shrink @(Enumeration D))

instance Arbitrary E where
  arbitrary = coerce (arbitrary @(Enumeration E))
  shrink = coerce (shrink @(Enumeration E))

instance Arbitrary F where
  arbitrary = coerce (arbitrary @(Enumeration F))
  shrink = coerce (shrink @(Enumeration F))

instance Arbitrary G where
  arbitrary = coerce (arbitrary @(Enumeration G))
  shrink = coerce (shrink @(Enumeration G))

instance Arbitrary H where
  arbitrary = coerce (arbitrary @(Enumeration H))
  shrink = coerce (shrink @(Enumeration H))

instance Arbitrary I where
  arbitrary = coerce (arbitrary @(Enumeration I))
  shrink = coerce (shrink @(Enumeration I))

instance Arbitrary J where
  arbitrary = coerce (arbitrary @(Enumeration J))
  shrink = coerce (shrink @(Enumeration J))

instance Arbitrary K where
  arbitrary = coerce (arbitrary @(Enumeration K))
  shrink = coerce (shrink @(Enumeration K))

newtype Enumeration a = Enumeration a

instance (Bounded a, Enum a, Eq a) => Arbitrary (Enumeration a) where
  arbitrary = fmap Enumeration arbitraryBoundedEnum
  shrink (Enumeration x) = if x == minBound
    then []
    else [Enumeration (pred x)]

