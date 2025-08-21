{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}

module Control.Monad.Indexed.Cont2 where

import Control.Additive
import Control.Applicative
import Control.Comonad
import Control.Monad
import Control.Monad.Indexed qualified as Indexed

newtype Cont2W w r r' a = Cont2W {runCont2W :: w (a -> r' -> r') -> r -> r}
  deriving stock (Functor)

deriving via (Indexed.FromIndexed (Cont2W w) r r) instance (Comonad w) => Applicative (Cont2W w r r)

deriving via (Indexed.FromIndexed (Cont2W w) r r) instance (Comonad w) => Monad (Cont2W w r r)

deriving via (Indexed.FromIndexed (Cont2W w) r r) instance (Comonad w) => Alternative (Cont2W w r r)

deriving via (Indexed.FromIndexed (Cont2W w) r r) instance (Comonad w) => MonadPlus (Cont2W w r r)

instance (Comonad w) => Indexed.Applicative (Cont2W w) where
  pure x = Cont2W $ \k -> extract k x

instance (Comonad w) => Indexed.Monad (Cont2W w) where
  Cont2W a >>= f = Cont2W $ \wk -> a (k' `extend` wk)
    where
      k' wk x = runCont2W (f x) wk

shift0 :: (Comonad w) => (w (a -> r' -> r') -> r -> Cont2W w r k k) -> Cont2W w r r' a
shift0 f = Cont2W $ \wk fl -> runCont2W (f wk fl) ((\_k -> \x _ -> x) <$> wk) fl

class (Indexed.Monad m) => Shifty m where
  shift :: ((a -> r' -> r') -> r -> m r k k) -> m r r' a

instance (Comonad w) => Shifty (Cont2W w) where
  shift :: (Comonad w) => ((a -> r' -> r') -> r -> Cont2W w r k k) -> Cont2W w r r' a
  shift f = shift0 $ \wk -> f (extract wk)

push :: (Shifty m) => a -> m i (a -> i) ()
push x = shift $ \k fl -> Indexed.pure $ k () (const fl) x

pop :: (Shifty m) => m (a -> i) i a
pop = shift $ \k fl -> Indexed.pure (\a -> k a (fl a))

run :: (Comonad w) => (w (a -> r) -> r) -> Cont2W w r r a
run act = shift0 $ \wk fl -> Indexed.pure $ act (fmap (\k x -> k x fl) wk)

run' :: (Comonad w) => (w r -> r) -> Cont2W w r r ()
run' act = shift0 $ \wk fl -> Indexed.pure $ act (fmap (\k -> k () fl) wk)

instance Additive (Cont2W w r r' a) where
  empty = Cont2W $ \_ fl -> fl
  (Cont2W a) <|> (Cont2W b) = Cont2W $ \wk fl -> a wk (b wk fl)

instance (Comonad w) => Indexed.Stacked (Cont2W w) where
  shift_ f = shift (\k -> f (k ()))
