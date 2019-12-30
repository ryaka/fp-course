{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}

module Course.Traversable where

import Course.Core
import Course.Functor
import Course.Applicative
import Course.List
import Course.ExactlyOne
import Course.Optional
import Course.Compose

-- | All instances of the `Traversable` type-class must satisfy three laws. These
-- laws are not checked by the compiler. These laws are given as:
--
-- * The law of naturality
--   `∀f g. f . traverse g ≅ traverse (f . g)`
--
-- * The law of identity
--   `∀x. traverse ExactlyOne x ≅ ExactlyOne x`
--
-- * The law of composition
--   `∀f g. traverse ((g <$>) . f) ≅ (traverse g <$>) . traverse f`
class Functor t => Traversable t where
  traverse ::
    Applicative k =>
    (a -> k b)
    -> t a
    -> k (t b)

instance Traversable List where
  traverse ::
    Applicative k =>
    (a -> k b)
    -> List a
    -> k (List b)
  traverse f =
    foldRight (\a b -> (:.) <$> f a <*> b) (pure Nil)

instance Traversable ExactlyOne where
  traverse ::
    Applicative k =>
    (a -> k b)
    -> ExactlyOne a
    -> k (ExactlyOne b)
  traverse fakb (ExactlyOne a) = ExactlyOne <$> (fakb a)

instance Traversable Optional where
  traverse ::
    Applicative k =>
    (a -> k b)
    -> Optional a
    -> k (Optional b)
  traverse _ Empty = pure Empty
  traverse fakb (Full a) = Full <$> (fakb a)

-- | Sequences a traversable value of structures to a structure of a traversable value.
--
-- >>> sequenceA (ExactlyOne 7 :. ExactlyOne 8 :. ExactlyOne 9 :. Nil)
-- ExactlyOne [7,8,9]
--
-- >>> sequenceA (Full (ExactlyOne 7))
-- ExactlyOne (Full 7)
--
-- >>> sequenceA (Full (*10)) 6
-- Full 60
sequenceA ::
  (Applicative k, Traversable t) =>
  t (k a)
  -> k (t a)
sequenceA = traverse id

instance (Traversable f, Traversable g) =>
  Traversable (Compose f g) where
-- Implement the traverse function for a Traversable instance for Compose
  traverse fakb (Compose fga) = Compose <$> traverse (traverse fakb) fga

-- | The `Product` data type contains one value from each of the two type constructors.
data Product f g a =
  Product (f a) (g a) deriving (Show, Eq)

instance (Functor f, Functor g) =>
  Functor (Product f g) where
-- Implement the (<$>) function for a Functor instance for Product
  (<$>) ::
    (a -> b)
    -> Product f g a
    -> Product f g b
  f <$> (Product fa ga) = Product (f <$> fa) (f <$> ga)

instance (Traversable f, Traversable g) =>
  Traversable (Product f g) where
-- Implement the traverse function for a Traversable instance for Product
  traverse ::
    Applicative k =>
    (a -> k b)
    -> Product f g a
    -> k (Product f g b)
  traverse fakb (Product fa ga) = lift2 Product (traverse fakb fa) (traverse fakb ga)

-- | The `Coproduct` data type contains one value from either of the two type constructors.
data Coproduct f g a =
  InL (f a)
  | InR (g a) deriving (Show, Eq)

instance (Functor f, Functor g) =>
  Functor (Coproduct f g) where
-- Implement the (<$>) function for a Functor instance for Coproduct
  (<$>) ::
    (a -> b)
    -> Coproduct f g a
    -> Coproduct f g b
  fab <$> InL fa = InL $ fab <$> fa
  fab <$> InR ga = InR $ fab <$> ga

instance (Traversable f, Traversable g) =>
  Traversable (Coproduct f g) where
-- Implement the traverse function for a Traversable instance for Coproduct
  traverse :: Applicative k =>
    (a -> k b)
    -> Coproduct f g a
    -> k (Coproduct f g b)
  traverse fakb (InL fa) = InL <$> (traverse fakb fa)
  traverse fakb (InR ga) = InR <$> (traverse fakb ga)
