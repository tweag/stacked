{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QualifiedDo #-}

-- | This module defines a 2-continuation flavour of indexed monads. Having two
-- continuation allows unrestricted backtracking (hence the 'Additive' instance
-- for 'Cont2W' below).
--
-- Most of this module is focused on using the indexed continuation monads for
-- delimited control, hence adopts its terminology. For instance in `'Cont2' w r
-- r'`, `r` and `r'` are called the input and output answer types respectively.
-- They are to be thought as a stack (a stack of `a -> b -> r` being a stack
-- whose two first elements are of type `a` and `b`).
--
-- 'Cont2W' is an indexed monad because the answer type can change along the
-- computation. This is called /answer-type modification/ in the delimited
-- control literature.
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

-- | 'shift0' is the most general delimited control operation on 'Cont2W'.
--
-- == __About the name__
-- It's named by analogy with the eponymous delimited control operation from the
-- delimited control literature (see [A Dynamic Interpretation of the CPS
-- Hierarchy](https://link.springer.com/chapter/10.1007/978-3-642-35182-2_21)).
-- The traditional @shift0@ captures all the continuations introduced by nested
-- resets; here we think of the comonad `w` as carrying the information of
-- additional resets.
shift0 :: (Comonad w) => (w (a -> r' -> r') -> r -> Cont2W w r k k) -> Cont2W w r r' a
shift0 f = Cont2W $ \wk fl -> runCont2W (f wk fl) ((\_k x _ -> x) <$> wk) fl

-- | A type class abstracting over delimited control capability for this
-- 2-continuation flavour of monads.
class (Stacked m) => Shifty m where
  shift :: ((a -> r' -> r') -> r -> m r k k) -> m r r' a

instance (Comonad w) => Shifty (Cont2W w) where
  shift :: (Comonad w) => ((a -> r' -> r') -> r -> Cont2W w r k k) -> Cont2W w r r' a
  shift f = shift0 $ \wk -> f (extract wk)

instance (Shifty f, Shifty g) => Shifty (f Indexed.:*: g) where
  shift f = (shift (\k fl -> Indexed.fst_star (f k fl))) Indexed.:*: (shift (\k fl -> Indexed.snd_star (f k fl)))

-- | 'pop' pops and returns the top element of the stack. See also the less
-- expressive 'pop_' which doesn't require a full 'Shifty' instance.
pop :: (Indexed.Applicative m, Shifty m) => m (a -> i) i a
pop = shift $ \k fl -> Indexed.pure (\a -> k a (fl a))

-- | @'yield' act@ runs the comonadically effectful action @act@ in the 'Cont2W'
-- monad. See also 'yield_'.
yield :: (Comonad w) => (w (a -> r) -> r) -> Cont2W w r r a
yield act = shift0 $ \wk fl -> Indexed.pure $ act (fmap (\k x -> k x fl) wk)

-- | @'yield_' act@ runs the comonadically effectful action @act@ in the
-- 'Cont2W' monad. This is a variant of 'yield' specialised to the case where
-- the action doesn't return a value.
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

-- | Restricted delimited control where continuations can be captured but can't
-- return values. This is enough for most stack-manipulating functions (the
-- stack, however, cannot influence the control flow anymore). Compared to
-- 'Shifty' it captures the additional case where the stack doesn't actually
-- exist at runtime ('Indexed.IgnoreIndices').
class Stacked m where
  shift_ :: ((r' -> r') -> r -> m r r'' r'') -> m r r' ()

instance (Stacked f, Stacked g) => Stacked (f Indexed.:*: g) where
  shift_ f = (shift_ (\s fl' -> Indexed.fst_star (f s fl'))) Indexed.:*: (shift_ (\s fl' -> Indexed.snd_star (f s fl')))

instance (Prelude.Applicative m) => Stacked (Indexed.IgnoreIndices m) where
  shift_ _ = Indexed.IgnoreIndices $ Prelude.pure ()

-- | 'stack' is a partial mapping operation on the stack.
--
-- In a first approximation it can be thought of as a function with type
--
-- > :: (j -> i) -> m i j ()
--
-- The direction of the arrow is explained by the representation of stack types
-- as function. A function with type
--
-- > ((d -> e -> r) -> (a -> b -> c -> r))
--
-- actually maps three elements @a, b, c@ to two elements @d, e@.
--
-- The additional parameter of type @i@ is a failure continuation. For instance
-- the following always fails to modify the stack:
--
-- > stack (\fl _ -> fl)
--
-- Finally, the extra function of type @(i -> j)@ is called an “unrolling
-- function” and is tasked with restoring the stack in its original state if a
-- later failure causes backtracking.
stack :: (Indexed.Applicative m, Stacked m) => (i -> j -> i) -> (i -> j) -> m i j ()
stack f unr = shift_ $ \k fl -> Indexed.pure $ f fl (k (unr fl))

(@) :: (Indexed.Applicative m, Stacked m) => m (a -> i) j b -> a -> m i j b
act @ a = push a *> act

infixl 9 @

push :: (Indexed.Applicative m, Stacked m) => a -> m i (a -> i) ()
push a = shift_ $ \k fl -> Indexed.pure (k (const fl) a)

-- | 'pop_' discards the top element of the stack. It is a less expressive
-- variant of 'pop', but 'pop' requires a 'Shifty' applicative, whereas 'pop_'
-- works on any stacked applicative.
pop_ :: (Indexed.Applicative m, Stacked m) => m (a -> i) i ()
pop_ = shift_ $ \k fl -> Indexed.pure (\a -> k (fl a))

-- | @'some' act@ iterates 1 or more times the action @act@ until it fails or
-- run out of input on the stack. See also 'many' which doesn't fail if it can't
-- iterate at least once.
some :: (Indexed.Alternative m, Stacked m) => (forall r'. m (a -> r') r' b) -> m ([a] -> r) r [b]
some a = Indexed.do
  stack uncons (\k x xs -> k (x : xs))
  (:) <$> a <*> many a
  where
    uncons fl _k [] = fl []
    uncons _fl k (x : xs) = k x xs

-- | @'many' act@ iterates 0 or more times the action @act@ until it fails or
-- run out of input on the stack. See also 'some' which iterates at least once.
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
