module Masna3.Server.Error where

import Data.Aeson
import GHC.Generics (Generic)
import Masna3.Api.File.FileId
import Servant (ServerError (..), err500)

data Masna3Error
  = TooManyRows Text
  | FileNotFound FileId
  | InvalidTransition InvalidTransitionError
  deriving stock (Eq, Ord, Show)

data InvalidTransitionError
  = NotPendingToUploaded FileId
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (ToJSON)

toServerError :: Masna3Error -> ServerError
toServerError = \case
  TooManyRows t ->
    err500
      { errBody =
          encode
            ErrorResponse
              { error = "TooManyRows"
              , context = Just $ object ["ownerId" .= t]
              }
      }
  FileNotFound t ->
    err500
      { errBody =
          encode
            ErrorResponse
              { error = "FileNotFound"
              , context = Just $ object ["fileId" .= t]
              }
      }
  InvalidTransition (NotPendingToUploaded t) ->
    err500
      { errBody =
          encode
            ErrorResponse
              { error = "InvalidTransition NotPendingToUploaded"
              , context = Just $ object ["fileId" .= t]
              }
      }

data ErrorResponse = ErrorResponse
  { error :: Text
  , context :: Maybe Value
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON)
