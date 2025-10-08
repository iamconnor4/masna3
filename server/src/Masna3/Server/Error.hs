module Masna3.Server.Error where

import Data.Aeson
import Deriving.Aeson
import Masna3.Api.File.FileId
import Servant (ServerError (..), err404, err500)

newtype FileNotFound = FileNotFound {fileId :: FileId}
  deriving stock (Eq, Generic, Ord, Show)
  deriving
    (FromJSON, ToJSON)
    via (CustomJSON '[FieldLabelModifier '[CamelToSnake], SumObjectWithSingleField] FileNotFound)

newtype MkInvalidTransitionFile = MkInvalidTransitionFile {fileId :: FileId}
  deriving stock (Eq, Generic, Ord, Show)
  deriving
    (FromJSON, ToJSON)
    via (CustomJSON '[FieldLabelModifier '[CamelToSnake], SumObjectWithSingleField] MkInvalidTransitionFile)

data Masna3Error
  = TooManyRows Text
  | FileNotFoundError FileNotFound
  | InvalidTransition InvalidTransitionError
  deriving stock (Eq, Generic, Ord, Show)
  deriving
    (FromJSON, ToJSON)
    via (CustomJSON '[FieldLabelModifier '[CamelToSnake], SumObjectWithSingleField] Masna3Error)

data InvalidTransitionError
  = NotPendingToUploaded MkInvalidTransitionFile
  deriving stock (Eq, Generic, Ord, Show)
  deriving
    (FromJSON, ToJSON)
    via (CustomJSON '[FieldLabelModifier '[CamelToSnake], SumObjectWithSingleField] InvalidTransitionError)

toServerError :: Masna3Error -> ServerError
toServerError = \case
  TooManyRows _ -> err500
  FileNotFoundError _ -> err404
  InvalidTransition _ -> err500
