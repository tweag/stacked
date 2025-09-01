{-# LANGUAGE ImpredicativeTypes #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}

-- | Import and use with `OverloadedLabels` to use labels as name for (generic)
-- constructor leads. That is `#MkT` instead of `lead @"MkT"`.
--
-- Because of the very generic type of the 'IsLabel' instance, importing this
-- module will probably interfere with other uses of `OverloadedLabels`.
module Control.Monad.Indexed.Cont2.Lead.Labels where

import Control.Monad.Indexed qualified as Indexed
import Control.Monad.Indexed.Cont2 qualified as Cont2
import Control.Monad.Indexed.Cont2.Lead.Generic
import GHC.Generics
import GHC.OverloadedLabels

instance
  ( Leading c t,
    Indexed.MonadPlus m,
    Cont2.Stacked m,
    s ~ (CFieldsType c (Rep t ()) r),
    u ~ (CFieldsType c (Rep t ()) t)
  ) =>
  IsLabel c (m (t -> r) s u)
  where
  fromLabel = lead @c @t @r @m
