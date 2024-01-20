{-# LANGUAGE DataKinds #-}

module Evolve where

import Brain (Brain (..))
import Numeric.LinearAlgebra.Static (R)
import Numeric.LinearAlgebra.Static.Squashable (squash, unsquash)

type Genome = R 12

brainToGenome :: Brain -> Genome
brainToGenome = squash . unBrain

genomeToBrain :: Genome -> Brain
genomeToBrain = Brain . unsquash
