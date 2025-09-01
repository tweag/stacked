-- | This module defines a variant of the 'Control.Applicative.Alternative' and
-- 'Control.Monad.MonadPlus' type class. Thanks to quantified constraints,
-- there's really nothing preventing us, in modern Haskell, to define it once
-- and for all at every arity.
--
-- Effectively, this is just a clone of the 'Monoid' type class. The reason for
-- preferring to define another class is that applicatives have a natural Monoid
-- instance:
--
-- > instance (Applicative f, Monoid a) => Applicative (f a) where
-- >   mempty = pure mempty
-- >   (<>) = liftA2 (<>)
--
-- In Base, the 'Monoid' instances for 'ST', 'IO', and 'STM' are defined like
-- this, and that of 'Maybe a' and 'Cont a b' are of a similar nature. See also
-- 'Data.Monoid.Ap'.
--
-- So we typically want a second one. An approach would be to give a monoid
-- instance to a newtype-wrapped version of our functor gadget. But that's very
-- syntactically heave (see 'Adding', below, though). So instead we propose a
-- dedicated type class, named after the fact that 'Alternative' function have
-- an additive flavour to them (`msum`, etc…), and situated in the `Control`
-- hierarchy to represent that it's intended to represent choice between
-- computations.
module Control.Additive
  ( Additive (..),
    sum,

    -- * Additives as monoids
    Adding (..),

    -- * Deriving-via combinators
    Monoidaly (..),
    Alternatively (..),
  )
where

import Control.Applicative qualified as Applicative
import Prelude hiding (sum)

class Additive a where
  empty :: a
  (<|>) :: a -> a -> a
  infixl 3 <|>

-- | Iterates '(<|>)' over a container.
sum :: (Additive a, Foldable f) => f a -> a
sum = foldr (<|>) empty

-- | Make any 'Additive' into a 'Monoid', and leverage any monoid functions.
newtype Adding a = Adding a

instance (Additive a) => Monoid (Adding a) where
  mempty = Adding empty

instance (Additive a) => Semigroup (Adding a) where
  Adding a <> Adding b = Adding (a <|> b)

-- | Derive 'Additive' instances with deriving via
newtype Monoidaly a = Monoidaly a

instance (Monoid a) => Additive (Monoidaly a) where
  empty = Monoidaly mempty
  Monoidaly a <|> Monoidaly b = Monoidaly (a <> b)

-- | Derive 'Additive' instances with deriving via
newtype Alternatively f a = Alternatively (f a)

instance (Applicative.Alternative f) => Additive (Alternatively f a) where
  empty = Alternatively Applicative.empty
  Alternatively a <|> Alternatively b = Alternatively (a Applicative.<|> b)
