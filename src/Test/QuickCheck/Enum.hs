{-# language TypeApplications #-}

module Test.QuickCheck.Enum
  ( A(..)
  , B(..)
  , C(..)
  , D(..)
  , E(..)
  , F(..)
  , G(..)
  , H(..)
  , I(..)
  , J(..)
  , K(..)
  ) where

import Data.Coerce (coerce)
import Test.QuickCheck (Arbitrary(..), arbitraryBoundedEnum)

data A = A0
  deriving (Eq,Ord,Show,Read,Bounded,Enum)

instance Arbitrary A where
  arbitrary = coerce (arbitrary @(Enumeration A))
  shrink = coerce (shrink @(Enumeration A))

data B = B0 | B1
  deriving (Eq,Ord,Show,Read,Bounded,Enum)

instance Arbitrary B where
  arbitrary = coerce (arbitrary @(Enumeration B))
  shrink = coerce (shrink @(Enumeration B))

data C = C0 | C1 | C2
  deriving (Eq,Ord,Show,Read,Bounded,Enum)

instance Arbitrary C where
  arbitrary = coerce (arbitrary @(Enumeration C))
  shrink = coerce (shrink @(Enumeration C))

data D = D0 | D1 | D2 | D3
  deriving (Eq,Ord,Show,Read,Bounded,Enum)

instance Arbitrary D where
  arbitrary = coerce (arbitrary @(Enumeration D))
  shrink = coerce (shrink @(Enumeration D))

data E = E0 | E1 | E2 | E3 | E4
  deriving (Eq,Ord,Show,Read,Bounded,Enum)

instance Arbitrary E where
  arbitrary = coerce (arbitrary @(Enumeration E))
  shrink = coerce (shrink @(Enumeration E))

data F = F0 | F1 | F2 | F3 | F4 | F5
  deriving (Eq,Ord,Show,Read,Bounded,Enum)

instance Arbitrary F where
  arbitrary = coerce (arbitrary @(Enumeration F))
  shrink = coerce (shrink @(Enumeration F))

data G = G0 | G1 | G2 | G3 | G4 | G5 | G6
  deriving (Eq,Ord,Show,Read,Bounded,Enum)

instance Arbitrary G where
  arbitrary = coerce (arbitrary @(Enumeration G))
  shrink = coerce (shrink @(Enumeration G))

data H = H0 | H1 | H2 | H3 | H4 | H5 | H6 | H7
  deriving (Eq,Ord,Show,Read,Bounded,Enum)

instance Arbitrary H where
  arbitrary = coerce (arbitrary @(Enumeration H))
  shrink = coerce (shrink @(Enumeration H))

data I = I0 | I1 | I2 | I3 | I4 | I5 | I6 | I7 | I8
  deriving (Eq,Ord,Show,Read,Bounded,Enum)

instance Arbitrary I where
  arbitrary = coerce (arbitrary @(Enumeration I))
  shrink = coerce (shrink @(Enumeration I))

data J = J0 | J1 | J2 | J3 | J4 | J5 | J6 | J7 | J8 | J9
  deriving (Eq,Ord,Show,Read,Bounded,Enum)

instance Arbitrary J where
  arbitrary = coerce (arbitrary @(Enumeration J))
  shrink = coerce (shrink @(Enumeration J))

data K = K0 | K1 | K2 | K3 | K4 | K5 | K6 | K7 | K8 | K9 | K10
  deriving (Eq,Ord,Show,Read,Bounded,Enum)

instance Arbitrary K where
  arbitrary = coerce (arbitrary @(Enumeration K))
  shrink = coerce (shrink @(Enumeration K))

newtype Enumeration a = Enumeration a

instance (Bounded a, Enum a, Eq a) => Arbitrary (Enumeration a) where
  arbitrary = fmap Enumeration arbitraryBoundedEnum
  shrink (Enumeration x) = if x == minBound
    then []
    else [Enumeration (pred x)]

