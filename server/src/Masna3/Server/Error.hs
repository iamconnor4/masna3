module Masna3.Server.Error where

import Data.Aeson
import Masna3.Api.File.FileId
import Servant (ServerError (..), err404, err500)

data Masna3Error
  = TooManyRows Text
  | FileNotFound FileId
  | InvalidTransition InvalidTransitionError
  deriving stock (Eq, Ord, Show)

data InvalidTransitionError
  = NotPendingToUploaded FileId
  deriving stock (Eq, Ord, Show)

toServerError :: Masna3Error -> ServerError
toServerError = \case
  TooManyRows t -> err500{errBody = encode t}
  FileNotFound _ -> err404
  InvalidTransition (NotPendingToUploaded t) -> err500{errBody = encode t}
