Stacked
=======

_The modern indexed monad stack_

Stacked is an indexed monad (see _e.g._ [this description by Conor
McBride][mcbride-ixmonad]) library built from the ground up to use modern GHC
features (such as qualified do and quantified constraints).

For instance

- The monad hierarchy follows that of base with for instance `Applicative` and
  `Monad`. These don't use distinctive names `IxApplicative`, `IxMonad`, or
  `ixBind`: they use the standard `Applicative`, `Monad`, and `(>>=)` names from
  base, and are meant to be imported qualified, and typically use with the
  [qualified do notation][qualified-do].
- But there is no indexed functor class defined in this library because, thanks
  to quantified constraints, the standard functor class suffices. The functor
  super-class for indexed applicative looks like `(forall i j. Functor f i j) =>
  …`
- We also take the opportunity of growing the stack from scratch to define a
  single `Additive` class to replace `Alternative` and `MonadPlus`. We then
  define `Alternative` as
  ```haskell
  type Alternative m = (Applicative m, forall r r' a. Additive (m r r' a))
  ```
  Which uses both quantified constraints and impredicative types.

The library is meant to feel familiar to Haskell programmers, despite the
clean-ups that modern Haskell affords.

## Points of interest

- Stacked has a rich sublibrary on continuation indexed monads and their
  relation with delimited control. This part of the library was developed to
  serve the needs of the Pup library.

[mcbride-ixmonad]: https://stackoverflow.com/questions/28690448/what-is-indexed-monad#28696299
[qualified-do]: https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/qualified_do.html#extension-QualifiedDo
