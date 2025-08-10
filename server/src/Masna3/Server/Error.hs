module Masna3.Server.Error where

data Masna3Error
  = TooManyRows Text
  deriving stock (Eq, Ord, Show)
