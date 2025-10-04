module Masna3.Server.Error where

import Data.Aeson
import Servant (ServerError (..), err500)

data Masna3Error
  = TooManyRows Text
  deriving stock (Eq, Ord, Show)

toServerError :: Masna3Error -> ServerError
toServerError = \case
  TooManyRows t -> err500{errBody = encode t}
