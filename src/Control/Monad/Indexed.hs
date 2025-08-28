{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE QuantifiedConstraints #-}

module Control.Monad.Indexed where

import Control.Additive
import Control.Applicative qualified as Applicative
import Control.Monad qualified as Monad
import Prelude hiding (Applicative (..), Monad (..), MonadFail (..))
import Prelude qualified

class (forall i j. Functor (f i j), forall i. Prelude.Applicative (f i i)) => Applicative f where
  pure :: a -> f i i a

  (<*>) :: f i j (a -> b) -> f j k a -> f i k b
  default (<*>) :: (Monad f) => f i j (a -> b) -> f j k a -> f i k b
  ff <*> aa = ff >>= \f -> aa >>= \a -> pure (f a)
  infixl 4 <*>

  liftA2 :: (a -> b -> c) -> f i j a -> f j k b -> f i k c
  liftA2 f x = (<*>) (fmap f x)

  (*>) :: f i j a -> f j k b -> f i k b
  a1 *> a2 = (id <$ a1) <*> a2
  infixl 4 *>

  (<*) :: f i j a -> f j k b -> f i k a
  (<*) = liftA2 const
  infixl 4 <*

class (Applicative m, forall i. Prelude.Monad (m i i)) => Monad m where
  (>>=) :: m i j a -> (a -> m j k b) -> m i k b

-- | This class is mainly used for the qualified `do`-notation, as described in
-- the documentation for 'Prelude.MonadFail'. Occasionally used to fail with an
-- error message in monads which support it, see for instance 'guardF' below.
class (Monad m) => MonadFail m where
  fail :: String -> m i j a

guardF :: (MonadFail m) => Bool -> String -> m i i ()
guardF True _ = pure ()
guardF False msg = fail msg

type Alternative m = (Applicative m, forall r r' a. Additive (m r r' a))

guard :: (Alternative m) => Bool -> m i i ()
guard True = pure ()
guard False = empty

type MonadPlus m = (Monad m, forall r r' a. Additive (m r r' a))

-- | For `QualifiedDo` notation
(>>) :: (Applicative m) => m i j () -> m j k a -> m i k a
(>>) = (*>)

class Stacked m where
  -- Alternative: we could have an Applicative modality `S m`, and a richer shift
  -- shiftm :: ((S m a -> r' -> r') -> r -> m r r'' r'') -> m r r' (S m a)
  --
  -- `S Print = Identity`, and `S Parse = Const ()`. The latter is why we need
  -- the modality (and why it'd need to be an attached type family). It's kind
  -- of cool. And you could define
  --
  -- pop :: m (a -> i) i (S m a)
  -- push :: S m a -> m i (a -> i)
  --
  -- But is uses a type family, which is always a little bit annoying I suppose.
  --
  -- I'm not sure it adds much expressive power, though. It wouldn't be terribly
  -- convenient to use a value behind such a modality. You might as well just
  -- use `shift'` honestly.
  shift_ :: ((r' -> r') -> r -> m r r'' r'') -> m r r' ()

stack :: (Applicative m, Stacked m) => (i -> j -> i) -> (i -> j) -> m i j ()
stack f unr = shift_ $ \k fl -> pure $ f fl (k (unr fl))

(@) :: (Applicative m, Stacked m) => m (a -> i) j b -> a -> m i j b
act @ a = stack (\_ s -> s a) (\s _ -> s) *> act

infixl 9 @

push :: (Applicative m, Stacked m) => a -> m i (a -> i) ()
push a = shift_ $ \k fl -> pure (k (const fl) a)

pop_ :: (Applicative m, Stacked m) => m (a -> i) i ()
pop_ = shift_ $ \k fl -> pure (\a -> k (fl a))

some :: (Alternative m, Stacked m) => (forall r'. m (a -> r') r' b) -> m ([a] -> r) r [b]
some a = Control.Monad.Indexed.do
  stack uncons (\k x xs -> k (x : xs))
  (:) <$> a <*> many a
  where
    uncons fl _k [] = fl []
    uncons _fl k (x : xs) = k x xs

many :: (Alternative m, Stacked m) => (forall r'. m (a -> r') r' b) -> m ([a] -> r) r [b]
many a = some a <|> (pop_ *> pure [])

optional :: (Alternative m, Stacked m) => (forall r'. m (a -> r') r' b) -> m (Maybe a -> r) r (Maybe b)
optional a =
  (justLead <*> a) <|> nothingLead
  where
    -- TODO: reorder module so that these can be derived.
    justLead = Control.Monad.Indexed.do
      stack (\cases _ k (Just x) -> k x; fl _ b -> fl b) (\k x -> k (Just x))
      pure Just
    nothingLead = Control.Monad.Indexed.do
      stack (\cases _ k Nothing -> k; fl _ b -> fl b) (\k -> k Nothing)
      pure Nothing

sepBy :: (MonadPlus m, Stacked m) => (forall r'. m (a -> r') r' b) -> (forall r'. m r' r' ()) -> m ([a] -> r) r [b]
sepBy p sep = Control.Monad.Indexed.do
  -- TODO: I don't know whether cheating on the unroll can be a
  -- problem. Replacing by a direct call to shift would solve that I suppose.
  stack (\cases _ k (x : xs) -> k (Just x) xs; _ k [] -> k Nothing (error "Why did this not pop?")) (\k _x xs -> k xs)
  r <- optional p
  case r of
    Nothing -> pop_ >> pure []
    Just x -> Control.Monad.Indexed.do
      (x :) <$> many (sep *> p)

newtype IgnoreStack m i j a = IgnoreStack {unIgnoreStack :: m a}
  deriving newtype
    ( Functor,
      Prelude.Applicative,
      Prelude.Monad,
      Applicative.Alternative,
      Monad.MonadPlus
    )

instance (Prelude.Applicative f) => Applicative (IgnoreStack f) where
  pure a = IgnoreStack $ Prelude.pure a
  IgnoreStack f <*> IgnoreStack a = IgnoreStack $ f Prelude.<*> a

instance (Prelude.Monad m) => Monad (IgnoreStack m) where
  IgnoreStack a >>= k = IgnoreStack $ a Prelude.>>= \x -> unIgnoreStack (k x)

instance (Applicative.Alternative m) => Additive (IgnoreStack m r r' a) where
  empty = IgnoreStack Applicative.empty
  (IgnoreStack a) <|> (IgnoreStack b) = IgnoreStack $ a Applicative.<|> b

instance (Prelude.Applicative m) => Stacked (IgnoreStack m) where
  shift_ _ = IgnoreStack $ Prelude.pure ()

instance (Prelude.MonadFail m) => MonadFail (IgnoreStack m) where
  fail msg = IgnoreStack $ Prelude.fail msg

data (:*:) f g i j a = (:*:) (f i j a) (g i j a)
  deriving stock (Functor)

-- ⚠️ All the patterns on `x :*: y` must be lazy lest definitions like `many` start
-- looping because they become strict in their recursive arguments.
fst_star :: (f :*: g) i j a -> f i j a
fst_star ~(x :*: _) = x

snd_star :: (f :*: g) i j a -> g i j a
snd_star ~(_ :*: y) = y

instance (Prelude.Applicative (f i j), Prelude.Applicative (g i j)) => Prelude.Applicative ((f :*: g) i j) where
  pure a = (Prelude.pure a) :*: (Prelude.pure a)
  ~(f :*: f') <*> (a :*: a') = (f Prelude.<*> a) :*: (f' Prelude.<*> a')

instance (Applicative f, Applicative g) => Applicative (f :*: g) where
  pure a = (pure a) :*: (pure a)

  ~(f :*: f') <*> ~(a :*: a') = (f <*> a) :*: (f' <*> a')
  ~(a :*: a') *> ~(b :*: b') = (a *> b) :*: (a' *> b')
  ~(a :*: a') <* ~(b :*: b') = (a <* b) :*: (a' <* b')

instance (Prelude.Monad (f i j), Prelude.Monad (g i j)) => Prelude.Monad ((f :*: g) i j) where
  ~(a :*: b) >>= k =
    (a Prelude.>>= \x -> let (r :*: _) = k x in r)
      :*: (b Prelude.>>= \y -> let (_ :*: s) = k y in s)

instance (Monad f, Monad g) => Monad (f :*: g) where
  ~(a :*: b) >>= k =
    (a >>= \x -> let (r :*: _) = k x in r)
      :*: (b >>= \y -> let (_ :*: s) = k y in s)

instance (MonadFail f, MonadFail g) => MonadFail (f :*: g) where
  fail msg = fail msg :*: fail msg

instance (Applicative.Alternative (f i j), Applicative.Alternative (g i j)) => Applicative.Alternative ((f :*: g) i j) where
  empty = Applicative.empty :*: Applicative.empty
  ~(a :*: a') <|> ~(b :*: b') = (a Applicative.<|> b) :*: (a' Applicative.<|> b')

instance (Monad.MonadPlus (f i j), Monad.MonadPlus (g i j)) => Monad.MonadPlus ((f :*: g) i j)

instance (Additive (f r r' a), Additive (g r r' a)) => Additive ((f :*: g) r r' a) where
  empty = empty :*: empty
  ~(a :*: a') <|> ~(b :*: b') = (a <|> b) :*: (a' <|> b')

instance (Stacked f, Stacked g) => Stacked (f :*: g) where
  shift_ f = (shift_ (\s fl' -> fst_star (f s fl'))) :*: (shift_ (\s fl' -> snd_star (f s fl')))

-- | A deriving via combinator
newtype FromIndexed m i j a = FromIndexed (m i j a)
  deriving (Functor)

instance (Applicative m, i ~ j) => Prelude.Applicative (FromIndexed m i j) where
  pure x = FromIndexed $ pure x
  (FromIndexed f) <*> (FromIndexed a) = FromIndexed $ f <*> a

instance (Monad m, i ~ j) => Prelude.Monad (FromIndexed m i j) where
  (FromIndexed a) >>= k =
    FromIndexed $
      a >>= \x ->
        let (FromIndexed b) = k x in b

instance (Alternative m, i ~ j) => Applicative.Alternative (FromIndexed m i j) where
  empty = FromIndexed empty
  (FromIndexed a) <|> (FromIndexed b) = FromIndexed $ a <|> b

instance (MonadPlus m, i ~ j) => Monad.MonadPlus (FromIndexed m i j)
