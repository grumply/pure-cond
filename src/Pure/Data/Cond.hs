{-# LANGUAGE CPP, UndecidableInstances, TypeSynonymInstances, FlexibleInstances, OverloadedStrings, DefaultSignatures, PatternSynonyms, ViewPatterns, TypeOperators, FlexibleContexts #-}
module Pure.Data.Cond where

import Pure.Data.Default
import Pure.Data.JSON
import Pure.Data.Try
import Pure.Data.Txt as Txt
import Pure.Data.View

import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set

import qualified Data.HashMap.Strict as HM

import qualified Data.Vector as Vector

import Control.Applicative
import Control.Exception
import Data.Complex
import Data.Int
import Data.Monoid
import Data.Ratio
import Data.Word

import GHC.Generics

-- | Cond, for conditional, is a class used for representing, and checking for,
-- nil values.
--
-- Cond is lawless, but instances should satisfy the ideal that a nil value
-- represents either failure, or as little information as possible.
--
-- Caveat emptor: Cond usage /will/ introduce bugs.
--
-- For `Any` and `All`, Cond's nil represents failure:
--
-- > (mempty :: Any) == Any False == (nil :: Any)
--
-- > (mempty :: All) == All True /= All False == (nil :: All)
--
-- For numbers, Cond's nil is zero.
--
-- For containers, Cond's nil is the empty container.
--
-- For text, Cond's nil is the empty text.
--
-- For Product and Sum, Cond's nil is the lited nil.
--
-- > (nil :: Cond a => Product a) == Product nil
--
-- > (nil :: Cond a => Sum a) == Sum nil
--
-- For Ordering:
--
-- > (nil :: Ordering) == LT
--
-- For Either:
--
-- > (nil :: Cond a => Either a b) == Left nil
--
-- For SomeException:
--
-- > (nil :: SomeException) == toException GenericException
-- > (isNil :: SomeException -> Bool) == True
-- > (notNil :: SomeException -> Bool) == False
--
class Cond a where
  nil :: a
  default nil :: (Generic a, GCond (Rep a)) => a
  nil = to gnil

  isNil :: a -> Bool
  default isNil :: (Generic a, GCond (Rep a)) => a -> Bool
  isNil = gisNil . from

  notNil :: a -> Bool
  notNil = not . isNil

infixr 1 ?
(?) :: (Cond x) => x -> a -> a -> a
(?) x t e = if notNil x then t else e

infixr 1 !?
(!?) :: (Cond x) => x -> a -> a -> a
(!?) x t e = if isNil x then t else e

may :: Cond a => (b -> a) -> Maybe b -> a
may = maybe nil

infixr 6 #
(#) :: (Cond x, Default a) => x -> a -> a
(#) b t = b Pure.Data.Cond.? t $ def

cond :: (Cond a, Default a) => a -> a
cond a = a # a

instance Cond [a] where
  nil = []
  isNil [] = True
  isNil _ = False

instance Cond a => Cond (Either a b) where
  nil = Left nil
  isNil (Left a) = isNil a
  isNil _ = False

data Nil = Nil deriving (Show)
instance Exception Nil

instance Cond SomeException where
  nil = toException Nil
  isNil _ = True
  notNil _ = False

instance Cond (Maybe a) where
  nil = Nothing
  isNil Nothing = True
  isNil _ = False

instance Cond (Try a) where
  nil = Failed
  isNil Failed = True
  isNil _ = False

instance Cond () where
  nil = ()
  isNil _ = True

instance Cond Any where
  nil = Any False

instance Cond All where
  nil = All False

instance Cond Ordering where
  nil = LT

instance Cond (Last a) where
  nil = Last Nothing
  isNil (Last Nothing) = True
  isNil _ = False

instance Cond (First a) where
  nil = First Nothing
  isNil (First Nothing) = True
  isNil _ = False

instance (Cond a) => Cond (Sum a) where
  nil = Sum nil
  isNil (Sum a) = isNil a

instance (Cond a) => Cond (Product a) where
  nil = Product nil
  isNil (Product a) = isNil a

instance (Cond a) => Cond (Const a b) where
  nil = Const nil
  isNil (Const a) = isNil a

instance Cond (ZipList a) where
  nil = ZipList []
  isNil (ZipList []) = True
  isNil _ = False

instance (Cond a) => Cond (Dual a) where
  nil = Dual nil
  isNil (Dual d) = isNil d

instance Cond Bool where
  nil = False
  isNil = not

instance Cond Txt where
  nil = ""
  isNil = Txt.null

instance {-# OVERLAPPABLE #-} (Num n, Eq n) => Cond n where
  nil = fromInteger 0
  isNil = (== fromInteger 0)

instance (Cond a, Cond b) => Cond (a,b) where
  nil = (nil,nil)
  isNil (a,b) = isNil a && isNil b

instance (Cond a, Cond b, Cond c) => Cond (a,b,c) where
  nil = (nil,nil,nil)
  isNil (a,b,c) = isNil a && isNil b && isNil c

instance (Cond a, Cond b, Cond c, Cond d) => Cond (a,b,c,d) where
  nil = (nil,nil,nil,nil)
  isNil (a,b,c,d) = isNil a && isNil b && isNil c && isNil d

instance (Cond a, Cond b, Cond c, Cond d, Cond e) => Cond (a,b,c,d,e) where
  nil = (nil,nil,nil,nil,nil)
  isNil (a,b,c,d,e) = isNil a && isNil b && isNil c && isNil d && isNil e

instance (Cond a, Cond b, Cond c, Cond d, Cond e, Cond f) => Cond (a,b,c,d,e,f) where
  nil = (nil,nil,nil,nil,nil,nil)
  isNil (a,b,c,d,e,f) = isNil a && isNil b && isNil c && isNil d && isNil e && isNil f

instance (Cond a, Cond b, Cond c, Cond d, Cond e, Cond f, Cond g) => Cond (a,b,c,d,e,f,g) where
  nil = (nil,nil,nil,nil,nil,nil,nil)
  isNil (a,b,c,d,e,f,g) = isNil a && isNil b && isNil c && isNil d && isNil e && isNil f && isNil g

instance Cond (Set.Set a) where
  nil = Set.empty
  isNil = Set.null

instance Cond (Seq.Seq a) where
  nil = Seq.empty
  isNil = Seq.null

instance Cond (Map.Map a b) where
  nil = Map.empty
  isNil = Map.null

instance Cond (IntMap.IntMap a) where
  nil = IntMap.empty
  isNil = IntMap.null

instance Cond IntSet.IntSet where
  nil = IntSet.empty
  isNil = IntSet.null

instance {-# OVERLAPPABLE #-} Cond (HM.HashMap a b) where
  nil = HM.empty
  isNil = HM.null

instance Cond (Vector.Vector a) where
  nil = Vector.empty
  isNil = Vector.null

instance {-# OVERLAPPING #-} Cond Obj where
  nil = mempty
  isNil o =
#ifdef __GHCJS__
    Prelude.null $ objectAssocs o
#else
    o == mempty
#endif

instance {-# OVERLAPPING #-} Cond Value where
  nil =
#ifdef __GHCJS__
    nullValue
#else
    Null
#endif
  isNil =
#ifdef __GHCJS__
    (== nullValue)
#else
    (== Null)
#endif

instance Cond View where
  nil = NullView Nothing
  isNil (NullView _) = True
  isNil _ = False

class GCond f where
  gnil :: f a
  gisNil :: f a -> Bool

instance GCond V1 where
  gnil = undefined
  gisNil _ = True

instance GCond U1 where
  gnil = U1
  gisNil U1 = True

instance (Cond a) => GCond (K1 i a) where
  gnil = K1 nil
  gisNil (K1 a) = isNil a

instance (GCond a, GCond b) => GCond (a :*: b) where
  gnil = gnil :*: gnil
  gisNil (a :*: b) = gisNil a && gisNil b

instance (GCond a, GCond b) => GCond (a :+: b) where
  gnil = L1 gnil
  gisNil (L1 n) = gisNil n
  gisNil _ = False

instance (GCond a) => GCond (M1 i c a) where
  gnil = M1 gnil
  gisNil (M1 a) = gisNil a
