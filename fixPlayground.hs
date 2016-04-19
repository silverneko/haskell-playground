{-# LANGUAGE GADTs, KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

import Control.Monad
import Control.Applicative

-- Reverse apply
(-:) :: a -> (a -> b) -> b
(-:) = flip id

infixl 1 -:

-- type-level fixed point combinator
newtype Fix f = Fix (f (Fix f))

deriving instance Show (f (Fix f)) => Show (Fix f)
deriving instance Eq (f (Fix f)) => Eq (Fix f)

fix :: f (Fix f) -> Fix f
fix = Fix

unfix :: Fix f -> f (Fix f)
unfix (Fix f) = f

data ListF :: * -> * -> * where
  NilF :: ListF a b
  ConsF :: a -> b -> ListF a b
  deriving (Show, Eq)

instance Functor (ListF a) where
  fmap _ NilF = NilF
  fmap f (ConsF a b) = ConsF a (f b)

-- List is ListF's fixed point
-- A List data type defined without recursion
type List a = Fix (ListF a)

nil :: List a
nil = fix NilF

cons :: a -> List a -> List a
cons x xs = fix $ ConsF x xs

infixr 5 `cons`

fmap' :: (a -> b) -> List a -> List b
fmap' f xs =
  case unfix xs of
    NilF -> nil
    ConsF x xs' -> f x `cons` fmap' f xs'

head' :: List a -> a
head' (Fix (ConsF a _)) = a

tail' :: List a -> List a
tail' (Fix (ConsF _ b)) = b

list1 = 2 `cons` 5 `cons` 8 `cons` nil
list2 = fmap' show list1

data TreeF :: * -> * -> * where
  LeafF :: TreeF a b
  NodeF :: b -> a -> b -> TreeF a b
  deriving (Show, Eq)

type Tree a = Fix (TreeF a)

leaf :: Tree a
leaf = fix LeafF

node :: Tree a -> a -> Tree a -> Tree a
node lt a rt = fix $ NodeF lt a rt

tree1 = node (node (node leaf 1 leaf) 1 leaf) 2 (node (node (node leaf 3 (node leaf 5 leaf)) 8 leaf) 13 leaf)

-- Cofree can add annotation to a functor
-- e.g. annotate a AST, annotate the NatF type with Int
data Cofree f a = a :< (f (Cofree f a))

deriving instance (Show a, Show (f (Cofree f a))) => Show (Cofree f a)
deriving instance (Eq a, Eq (f (Cofree f a))) => Eq (Cofree f a)

infixr 5 :<

data NatF :: * -> * where
  OneF :: NatF a
  SuccF :: a -> NatF a
  deriving (Show, Eq)

type Nat = Cofree NatF Int

one :: Nat
one = 1 :< OneF

succ :: Nat -> Nat
succ f@(a :< _) = a + 1 :< SuccF f


