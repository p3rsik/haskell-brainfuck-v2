module Interpreter.Types.Program
  ( UnverifiedProgram (..),
    VerifiedProgram (..),
  ) where

import           Relude


-- | Newtype wrapper to encode the fact, that program is unverified(e.g. syntax was not checked)
newtype UnverifiedProgram = UnverifiedProgram { unUnverifiedProgram :: Text } deriving (Eq, Show)

-- | Newtype wrapper to encode the fact, that program is verified(e.g. syntax was checked)
newtype VerifiedProgram = VerifiedProgram { unVerifiedProgram :: Text } deriving (Eq, Show)
