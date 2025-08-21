module Control.Comonad.Weave where

import Control.Comonad (Comonad (..))
import Control.Comonad.Store qualified as Comonad

-- | This plays a similar role as `MonadPlus` and `Alternative` play for monads
-- and applicatives. This equips some of the hom sets of the co-kleisli category
-- with a monoid structure.
--
-- It's surprising that only some hom sets can be monoids, whereas `MonadPlus`
-- equips all hom-sets with a monoid structure. I don't have an good
-- explanation, to be honest.
class (Comonad w) => Weave w r where
  exit :: w a -> r
  weave :: (w a -> r) -> (w a -> r) -> w a -> r

instance (Comonad w, s ~ k) => Weave (Comonad.StoreT s w) k where
  exit wa = snd (Comonad.runStoreT wa)
  (kwl `weave` kwr) (Comonad.StoreT k s) = kwl (Comonad.StoreT k (kwr (Comonad.StoreT k s)))
