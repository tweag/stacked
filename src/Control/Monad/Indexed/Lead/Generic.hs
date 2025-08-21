{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Control.Monad.Indexed.Lead.Generic where

import Control.Monad.Indexed qualified as Indexed
import Data.Kind
import GHC.Generics
import GHC.TypeLits

-- Partly inspired by (and some code lifted from)
-- https://github.com/tweag/linear-dest/blob/0e7db2e6b24aad348837ac78d8137712c1d8d12a/src/Compact/Pure/Internal.hs
-- Partly inspired by generic lens
-- https://hackage.haskell.org/package/generic-lens-core-2.2.1.0/docs/src/Data.Generics.Sum.Internal.Constructors.html

type Leading :: Symbol -> Type -> Constraint
class Leading c t where
  match :: (t -> i) -> CFieldsType c (Rep t ()) i -> t -> i
  unmatch :: (t -> i) -> CFieldsType c (Rep t ()) i

-- the_constr :: CFieldsType c (Rep t ()) tgt

lead :: forall c t r m. (Leading c t, Indexed.MonadPlus m, Indexed.Stacked m) => m (t -> r) (CFieldsType c (Rep t ()) r) (CFieldsType c (Rep t ()) t)
lead = Indexed.do
  Indexed.stack (match @c @t @r) (unmatch @c @t @r)
  Indexed.pure $ unmatch @c @t @t id

------------------------------------------------------------------------------
--
-- Below, the generic machinery
--
------------------------------------------------------------------------------

instance (Generic t, Defined (Rep t) (TypeError (NoGeneric t)) (() :: Constraint), GLeading c (Rep t) ()) => Leading c t where
  match fl k t = gmatch @c @(Rep t) @() (fl . to) k (from t)
  unmatch k = gunmatch @c @(Rep t) @() (k . to)

type NoGeneric t = 'Text "No instance for " ':<>: QuoteType (Generic t)

type QuoteType t = 'Text "‘" ':<>: 'ShowType t ':<>: 'Text "’"

type GLeading :: Symbol -> (Type -> Type) -> Type -> Constraint
class GLeading c rep x where
  gmatch :: (rep x -> i) -> CFieldsType c (rep x) i -> rep x -> i
  gunmatch :: (rep x -> i) -> CFieldsType c (rep x) i

-- gthe_constr :: CFieldsType c rep t

instance (GSumLeading c (IsJust (SelectConstructor c (l x))) (SelectConstructor c ((l :+: r) x)) l r x) => GLeading c (l :+: r) x where
  gmatch fl k t =
    gsum_match @c @(IsJust (SelectConstructor c (l x))) @(SelectConstructor c ((l :+: r) x)) @l @r @x fl k t
  gunmatch k =
    gsum_unmatch @c @(IsJust (SelectConstructor c (l x))) @(SelectConstructor c ((l :+: r) x)) @l @r @x k

instance (GProdLeading f x) => GLeading c (M1 C ('MetaCons c fixity fields) f) x where
  gmatch _fl k (M1 t) = gapply k t
  gunmatch k = gbuild k

instance (GLeading c f x) => GLeading c (D1 meta f) x where
  gmatch fl k (M1 t) = gmatch @c @f @x (fl . M1) k t
  gunmatch (k :: D1 meta f x -> i) = gunmatch @c @f @x @i (k . M1)

type GSumLeading :: Symbol -> Bool -> Maybe Type -> (Type -> Type) -> (Type -> Type) -> Type -> Constraint
class GSumLeading c inl k l r x where
  gsum_match :: ((l :+: r) x -> i) -> FieldsType (FromJust k) i -> (l :+: r) x -> i
  gsum_unmatch :: ((l :+: r) x -> i) -> FieldsType (FromJust k) i

instance (GLeading c l x, SelectConstructor c (l x) ~ 'Just k) => GSumLeading c 'True ('Just k) l r x where
  gsum_match fl k (L1 tl) = gmatch @c @l @x (fl . L1) k tl
  gsum_match fl _k (R1 tr) = fl (R1 tr)

  gsum_unmatch k = gunmatch @c @l @x (k . L1)

instance (GLeading c r x, SelectConstructor c (r x) ~ 'Just k) => GSumLeading c 'False ('Just k) l r x where
  gsum_match fl _k (L1 tl) = fl (L1 tl)
  gsum_match fl k (R1 tr) = gmatch @c @r @x (fl . R1) k tr

  gsum_unmatch k = gunmatch @c @r @x (k . R1)

class GProdLeading t x where
  gapply :: FieldsType (t x) i -> t x -> i
  gbuild :: (t x -> i) -> FieldsType (t x) i

-- gconstr :: FieldsType (t x) tgt

instance (GProdLeading l x, GProdLeading r x) => GProdLeading (l :*: r) x where
  gapply k (l :*: r) = gapply (gapply k l) r
  gbuild (k :: (l :*: r) x -> i) = gbuild @l @x @(FieldsType (r x) i) (\l -> gbuild @r @x @i (\r -> k (l :*: r)))

instance (GProdLeading f x) => GProdLeading (M1 i meta f) x where
  gapply k (M1 x) = gapply k x
  gbuild k = gbuild (k . M1)

instance GProdLeading U1 x where
  gapply k _ = k
  gbuild k = k U1

instance GProdLeading (K1 i t) x where
  gapply k (K1 t) = k t
  gbuild k = k . K1

type SelectConstructor :: Symbol -> Type -> Maybe Type
type family SelectConstructor c rep where
  SelectConstructor c (C1 ('MetaCons c _ _) f p) = 'Just (f p)
  SelectConstructor c (C1 ('MetaCons _ _ _) _ _) = 'Nothing
  SelectConstructor c ((f :+: g) p) = SelectConstructor c (f p) <|> SelectConstructor c (g p)
  SelectConstructor c (V1 _) = 'Nothing
  SelectConstructor c (M1 _ _ f p) = SelectConstructor c (f p)
  SelectConstructor _ _ = TypeError ('Text "SelectConstructor: unexpected representation")

type (<|>) :: Maybe k -> Maybe k -> Maybe k
type family x <|> y where
  ('Just v) <|> _ = 'Just v
  'Nothing <|> y = y

type IsJust :: Maybe k -> Bool
type family IsJust x where
  IsJust ('Just v) = 'True
  IsJust 'Nothing = 'False

type FromJust :: Maybe k -> k
type family FromJust x where
  FromJust ('Just v) = v
  FromJust 'Nothing = TypeError ('Text "FromJust: This constructor doesn't seem to exist")

type FieldsType :: Type -> Type -> Type
type family FieldsType rep tgt where
  -- FieldsType (S1 _ f p) tgt = StripMetadata (f p) -> tgt
  FieldsType (U1 _) tgt = tgt
  FieldsType ((f :*: g) p) tgt = FieldsType (f p) (FieldsType (g p) tgt)
  FieldsType (M1 _ _ f p) tgt = FieldsType (f p) tgt
  FieldsType (K1 _ c _) tgt = c -> tgt
  FieldsType _ _ = TypeError ('Text "FieldsType: unexpected representation")

type family CFieldsType c rep tgt where
  CFieldsType c rep tgt = FieldsType (FromJust (SelectConstructor c rep)) tgt

------------------------------------------------------------------------------
--
-- Error messages
--
------------------------------------------------------------------------------

-- See https://blog.csongor.co.uk/report-stuck-families/ and
-- https://hackage.haskell.org/package/generic-lens-core-2.2.1.0/docs/Data-Generics-Internal-Errors.html#t:Defined
type family Defined (break :: Type -> Type) (err :: Constraint) (a :: k) :: k where
  Defined Apart _ _ = Daemon
  Defined _ _ k = k

data Apart a

type family Daemon :: k
