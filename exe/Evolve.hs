{-# LANGUAGE DataKinds #-}

module Evolve where

import Numeric.LinearAlgebra.Static (R)
import Numeric.LinearAlgebra.Static.Squashable (squash, unsquash)
import Types (Brain (..))

type Genome = R 12

brainToGenome :: Brain -> Genome
brainToGenome = squash . unBrain

genomeToBrain :: Genome -> Brain
genomeToBrain = Brain . unsquash
