{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE QualifiedDo #-}

module Control.Monad.Indexed.Cont where

import Control.Comonad
import Control.Comonad.Store
import Control.Monad.Indexed qualified as Indexed

newtype ContW w r r' a = ContW {runContW :: w (a -> r') -> r}
  deriving stock (Functor)

deriving via (Indexed.FromIndexed (ContW w) r r) instance (Comonad w) => Applicative (ContW w r r)

deriving via (Indexed.FromIndexed (ContW w) r r) instance (Comonad w) => Monad (ContW w r r)

instance (Comonad w) => Indexed.Applicative (ContW w) where
  pure x = ContW $ \k -> extract k x

instance (Comonad w) => Indexed.Monad (ContW w) where
  ContW a >>= f = ContW $ \wk -> a (k' `extend` wk)
    where
      k' wk x = runContW (f x) wk

shift :: (Comonad w) => (w (a -> r') -> ContW w r k k) -> ContW w r r' a
shift f = ContW $ \wk -> runContW (f wk) (const id <$> wk)

abort :: (Comonad w) => r -> ContW w r r' a
abort a = shift $ \_ -> Indexed.pure a

-- reset1 :: _ -- (Comonad w) => ContW w r r' r' -> ContW w r r r
capture :: (Comonad w) => ContW w b r' r' -> ContW w r r (w b)
capture (ContW a) = ContW $ \wk -> extract wk (a `extend` (const id <$> wk))

-- reset1 a = a Indexed.>>= (ContW id)
-- reset1 (ContW a) = ContW $ \wk -> wk (a id)

-- something :: (Comonad w) => ContW w r r' a -> ContW w (a -> r') r'' b -> ContW w r r'' b
-- something (ContW b) (ContW a) = ContW $ \wk -> b (extend a wk)

handle :: (Comonad w) => ContW (StoreT k w) r r' a -> ContW w k r' a -> ContW w r r' a
handle (ContW inner) (ContW handler) =
  ContW $ \wk -> inner (StoreT (const <$> wk) (handler wk))

pullback :: (Comonad w) => (forall x. w x -> v x) -> ContW v r r' a -> ContW w r r' a
pullback f (ContW a) = ContW $ \wk -> a (f wk)

run :: (Comonad w) => (w (a -> r) -> r) -> ContW w r r a
run act = shift $ Indexed.pure . act

run' :: (Comonad w) => (w r -> r) -> ContW w r r ()
run' act = shift $ \wk -> Indexed.pure $ act (($ ()) <$> wk)
