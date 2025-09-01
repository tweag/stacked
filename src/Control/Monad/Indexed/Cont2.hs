{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QualifiedDo #-}

module Control.Monad.Indexed.Cont2
  ( -- * Abstract delimited control
    Shifty (..),
    Stacked (..),
    stack,
    pop,
    pop_,
    push,
    (@),

    -- * Comonad-to-indexed-monad transformer
    Cont2W (..),
    shift0,
    yield,
    yield_,

    -- * Bidirectional variants of traditional combinators
    some,
    many,
    optional,
    sepBy,
  )
where

import Control.Additive
import Control.Applicative qualified as Applicative
import Control.Comonad
import Control.Monad qualified as Monad
import Control.Monad.Indexed ((*>), (<*>))
import Control.Monad.Indexed qualified as Indexed
import Prelude hiding (Applicative (..), Monad (..), MonadFail (..))
import Prelude qualified

newtype Cont2W w r r' a = Cont2W {runCont2W :: w (a -> r' -> r') -> r -> r}
  deriving stock (Functor)

deriving via (Indexed.FromIndexed (Cont2W w) r r) instance (Comonad w) => Prelude.Applicative (Cont2W w r r)

deriving via (Indexed.FromIndexed (Cont2W w) r r) instance (Comonad w) => Prelude.Monad (Cont2W w r r)

deriving via (Indexed.FromIndexed (Cont2W w) r r) instance (Comonad w) => Applicative.Alternative (Cont2W w r r)

deriving via (Indexed.FromIndexed (Cont2W w) r r) instance (Comonad w) => Monad.MonadPlus (Cont2W w r r)

instance (Comonad w) => Indexed.Applicative (Cont2W w) where
  pure x = Cont2W $ \k -> extract k x

instance (Comonad w) => Indexed.Monad (Cont2W w) where
  Cont2W a >>= f = Cont2W $ \wk -> a (k' `extend` wk)
    where
      k' wk x = runCont2W (f x) wk

shift0 :: (Comonad w) => (w (a -> r' -> r') -> r -> Cont2W w r k k) -> Cont2W w r r' a
shift0 f = Cont2W $ \wk fl -> runCont2W (f wk fl) ((\_k -> \x _ -> x) <$> wk) fl

class (Stacked m) => Shifty m where
  shift :: ((a -> r' -> r') -> r -> m r k k) -> m r r' a

instance (Comonad w) => Shifty (Cont2W w) where
  shift :: (Comonad w) => ((a -> r' -> r') -> r -> Cont2W w r k k) -> Cont2W w r r' a
  shift f = shift0 $ \wk -> f (extract wk)

instance (Shifty f, Shifty g) => Shifty (f Indexed.:*: g) where
  shift f = (shift (\k fl -> Indexed.fst_star (f k fl))) Indexed.:*: (shift (\k fl -> Indexed.snd_star (f k fl)))

pop :: (Indexed.Applicative m, Shifty m) => m (a -> i) i a
pop = shift $ \k fl -> Indexed.pure (\a -> k a (fl a))

yield :: (Comonad w) => (w (a -> r) -> r) -> Cont2W w r r a
yield act = shift0 $ \wk fl -> Indexed.pure $ act (fmap (\k x -> k x fl) wk)

yield_ :: (Comonad w) => (w r -> r) -> Cont2W w r r ()
yield_ act = shift0 $ \wk fl -> Indexed.pure $ act (fmap (\k -> k () fl) wk)

instance Additive (Cont2W w r r' a) where
  empty = Cont2W $ \_ fl -> fl
  (Cont2W a) <|> (Cont2W b) = Cont2W $ \wk fl -> a wk (b wk fl)

instance (Comonad w) => Stacked (Cont2W w) where
  shift_ f = shift (\k -> f (k ()))

----------------------------------------------------------------------------------------
--
-- Stacked
--
----------------------------------------------------------------------------------------

class Stacked m where
  shift_ :: ((r' -> r') -> r -> m r r'' r'') -> m r r' ()

instance (Stacked f, Stacked g) => Stacked (f Indexed.:*: g) where
  shift_ f = (shift_ (\s fl' -> Indexed.fst_star (f s fl'))) Indexed.:*: (shift_ (\s fl' -> Indexed.snd_star (f s fl')))

instance (Prelude.Applicative m) => Stacked (Indexed.IgnoreIndices m) where
  shift_ _ = Indexed.IgnoreIndices $ Prelude.pure ()

stack :: (Indexed.Applicative m, Stacked m) => (i -> j -> i) -> (i -> j) -> m i j ()
stack f unr = shift_ $ \k fl -> Indexed.pure $ f fl (k (unr fl))

(@) :: (Indexed.Applicative m, Stacked m) => m (a -> i) j b -> a -> m i j b
act @ a = stack (\_ s -> s a) (\s _ -> s) *> act

infixl 9 @

push :: (Indexed.Applicative m, Stacked m) => a -> m i (a -> i) ()
push a = shift_ $ \k fl -> Indexed.pure (k (const fl) a)

pop_ :: (Indexed.Applicative m, Stacked m) => m (a -> i) i ()
pop_ = shift_ $ \k fl -> Indexed.pure (\a -> k (fl a))

some :: (Indexed.Alternative m, Stacked m) => (forall r'. m (a -> r') r' b) -> m ([a] -> r) r [b]
some a = Indexed.do
  stack uncons (\k x xs -> k (x : xs))
  (:) <$> a <*> many a
  where
    uncons fl _k [] = fl []
    uncons _fl k (x : xs) = k x xs

many :: (Indexed.Alternative m, Stacked m) => (forall r'. m (a -> r') r' b) -> m ([a] -> r) r [b]
many a = some a <|> (pop_ *> Indexed.pure [])

optional :: (Indexed.Alternative m, Stacked m) => (forall r'. m (a -> r') r' b) -> m (Maybe a -> r) r (Maybe b)
optional a =
  (justLead <*> a) <|> nothingLead
  where
    -- TODO: reorder module so that these can be derived.
    justLead = Indexed.do
      stack (\cases _ k (Just x) -> k x; fl _ b -> fl b) (\k x -> k (Just x))
      Indexed.pure Just
    nothingLead = Indexed.do
      stack (\cases _ k Nothing -> k; fl _ b -> fl b) (\k -> k Nothing)
      Indexed.pure Nothing

sepBy :: (Indexed.MonadPlus m, Stacked m) => (forall r'. m (a -> r') r' b) -> (forall r'. m r' r' ()) -> m ([a] -> r) r [b]
sepBy p sep = Indexed.do
  -- TODO: I don't know whether cheating on the unroll can be a
  -- problem. Replacing by a direct call to shift would solve that I suppose.
  stack (\cases _ k (x : xs) -> k (Just x) xs; _ k [] -> k Nothing (error "Why did this not pop?")) (\k _x xs -> k xs)
  r <- optional p
  case r of
    Nothing -> pop_ *> Indexed.pure []
    Just x -> Indexed.do
      (x :) <$> many (sep *> p)
