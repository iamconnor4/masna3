module Masna3.Server.Error where

import Data.Aeson
import Deriving.Aeson
import Masna3.Api.File.FileId
import Masna3.Api.Owner.OwnerId
import Servant (ServerError (..), err404, err500)

newtype OwnerNotFound = OwnerNotFound {ownerId :: OwnerId}
  deriving stock (Eq, Generic, Ord, Show)
  deriving
    (FromJSON, ToJSON)
    via (CustomJSON '[FieldLabelModifier '[CamelToSnake], SumObjectWithSingleField] OwnerNotFound)

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
  | OwnerNotFoundError OwnerNotFound
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
  OwnerNotFoundError _ -> err404
  FileNotFoundError _ -> err404
  InvalidTransition _ -> err500
