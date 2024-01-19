{-# LANGUAGE DataKinds #-}

module Evolve where

import Brain (Brain (..))
import Numeric.LinearAlgebra.Static (R, col, split, (|||))
import Numeric.LinearAlgebra.Static.Squashable (squash)

type Genome = R 12

brainToGenome :: Brain -> Genome
brainToGenome = squash . unBrain

genomeToBrain :: Genome -> Brain
genomeToBrain genome =
  let (c1, c2) = split genome :: (R 6, R 6)
   in Brain $ col c1 ||| col c2
