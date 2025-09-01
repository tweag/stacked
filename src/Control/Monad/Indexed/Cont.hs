{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE QualifiedDo #-}

module Control.Monad.Indexed.Cont
  ( -- * Abstract delimited control
    Shifty (..),
    Stacked (..),
    stack,
    pop,
    pop_,
    push,
    (@),

    -- * Comonad-to-indexed-monad transformer
    ContW (..),
    shift0,
    yield,
    yield_,

    -- * Experimental combinators (subject to radical change)
    abort,
    capture,
    handle,
    pullback,
  )
where

import Control.Comonad
import Control.Comonad.Store
import Control.Monad.Indexed ((*>))
import Control.Monad.Indexed qualified as Indexed
import Prelude hiding (Applicative (..), Monad (..), MonadFail (..))
import Prelude qualified

newtype ContW w r r' a = ContW {runContW :: w (a -> r') -> r}
  deriving stock (Functor)

deriving via (Indexed.FromIndexed (ContW w) r r) instance (Comonad w) => Prelude.Applicative (ContW w r r)

deriving via (Indexed.FromIndexed (ContW w) r r) instance (Comonad w) => Prelude.Monad (ContW w r r)

instance (Comonad w) => Indexed.Applicative (ContW w) where
  pure x = ContW $ \k -> extract k x

instance (Comonad w) => Indexed.Monad (ContW w) where
  ContW a >>= f = ContW $ \wk -> a (k' `extend` wk)
    where
      k' wk x = runContW (f x) wk

shift0 :: (Comonad w) => (w (a -> r') -> ContW w r k k) -> ContW w r r' a
shift0 f = ContW $ \wk -> runContW (f wk) (const id <$> wk)

yield :: (Comonad w) => (w (a -> r) -> r) -> ContW w r r a
yield act = shift0 $ Indexed.pure . act

yield_ :: (Comonad w) => (w r -> r) -> ContW w r r ()
yield_ act = shift0 $ \wk -> Indexed.pure $ act (($ ()) <$> wk)

class (Stacked m) => Shifty m where
  shift :: ((a -> r') -> m r k k) -> m r r' a

instance (Comonad w) => Shifty (ContW w) where
  shift f = shift0 $ \wk -> f (extract wk)

class Stacked m where
  shift_ :: (r' -> m r r'' r'') -> m r r' ()

instance (Comonad w) => Stacked (ContW w) where
  shift_ f = shift (\k -> f (k ()))

stack :: (Indexed.Applicative m, Stacked m) => (j -> i) -> m i j ()
stack f = shift_ $ \k -> Indexed.pure $ f k

(@) :: (Indexed.Applicative m, Stacked m) => m (a -> i) j b -> a -> m i j b
act @ a = push a *> act

infixl 9 @

pop :: (Indexed.Applicative m, Shifty m) => m (a -> i) i a
pop = shift $ \k -> Indexed.pure (\a -> k a)

push :: (Indexed.Applicative m, Stacked m) => a -> m i (a -> i) ()
push a = shift_ $ \k -> Indexed.pure (k a)

pop_ :: (Indexed.Applicative m, Stacked m) => m (a -> i) i ()
pop_ = shift_ $ \k -> Indexed.pure (\_a -> k)

----------------------------------------------------------------------------------------
--
-- Experimental
--
----------------------------------------------------------------------------------------

abort :: (Indexed.Applicative m, Shifty m) => r -> m r r' a
abort a = shift $ \_ -> Indexed.pure a

capture :: (Comonad w) => ContW w b r' r' -> ContW w r r (w b)
capture (ContW a) = ContW $ \wk -> extract wk (a `extend` (const id <$> wk))

handle :: (Comonad w) => ContW (StoreT k w) r r' a -> ContW w k r' a -> ContW w r r' a
handle (ContW inner) (ContW handler) =
  ContW $ \wk -> inner (StoreT (const <$> wk) (handler wk))

pullback :: (Comonad w) => (forall x. w x -> v x) -> ContW v r r' a -> ContW w r r' a
pullback f (ContW a) = ContW $ \wk -> a (f wk)
