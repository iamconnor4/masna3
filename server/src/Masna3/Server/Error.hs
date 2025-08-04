module Masna3.Server.Error where

import Data.Text (Text)

data Masna3Error
  = TooManyRows Text
  deriving stock (Eq, Ord, Show)
