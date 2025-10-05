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
              { errorType = "TooManyRows"
              , context = Just $ object ["ownerId" .= t]
              }
      }
  FileNotFound t ->
    err500
      { errBody =
          encode
            ErrorResponse
              { errorType = "FileNotFound"
              , context = Just $ object ["fileId" .= t]
              }
      }
  InvalidTransition (NotPendingToUploaded t) ->
    err500
      { errBody =
          encode
            ErrorResponse
              { errorType = "InvalidTransition NotPendingToUploaded"
              , context = Just $ object ["fileId" .= t]
              }
      }

data ErrorResponse = ErrorResponse
  { errorType :: Text
  , context :: Maybe Value
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON)
