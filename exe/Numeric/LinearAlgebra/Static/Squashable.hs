{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Numeric.LinearAlgebra.Static.Squashable where

import GHC.TypeLits as T
import Numeric.LinearAlgebra.Static (L, R, splitCols, uncol, (#))

class (KnownNat n, KnownNat m) => Squashable n m where
  squash :: L n m -> R (n T.* m)

instance (KnownNat n) => Squashable n 1 where
  squash = uncol

instance
  {-# OVERLAPPABLE #-}
  ( KnownNat n,
    KnownNat m,
    KnownNat (n T.* (m - 1)),
    1 <= m,
    n + (n T.* (m - 1)) ~ (n T.* m),
    Squashable n (m - 1)
  ) =>
  Squashable n m
  where
  squash mat =
    let (c1, c2) = splitCols mat :: (L n 1, L n (m - 1))
     in squash c1 # squash c2
