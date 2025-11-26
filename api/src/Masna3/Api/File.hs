module Masna3.Api.File where

import Data.Aeson
import Data.Text
import Deriving.Aeson
import Servant.API

import Masna3.Api.File.FileId
import Masna3.Api.Owner.OwnerId

data FileRegistrationForm = FileRegistrationForm
  { fileName :: Text
  , ownerId :: OwnerId
  , mimeType :: Text
  }
  deriving stock (Eq, Generic, Ord, Show)
  deriving
    (FromJSON, ToJSON)
    via (CustomJSON '[FieldLabelModifier '[CamelToSnake], SumObjectWithSingleField])
          FileRegistrationForm

data FileRegistrationResult = FileRegistrationResult
  { fileId :: FileId
  , url :: Text
  }
  deriving stock (Eq, Generic, Ord, Show)
  deriving
    (FromJSON, ToJSON)
    via (CustomJSON '[FieldLabelModifier '[CamelToSnake], SumObjectWithSingleField])
          FileRegistrationResult

data UploadCancellationForm = UploadCancellationForm
  { fileId :: FileId
  }
  deriving stock (Eq, Generic, Ord, Show)
  deriving
    (FromJSON, ToJSON)
    via (CustomJSON '[FieldLabelModifier '[CamelToSnake], SumObjectWithSingleField])
          UploadCancellationForm

type RegisterFile =
  Summary "Register a file for upload"
    :> "register"
    :> ReqBody '[JSON] FileRegistrationForm
    :> Post '[JSON] FileRegistrationResult

type ConfirmFileUpload =
  Summary "Register a file for upload"
    :> Capture "file_id" FileId
    :> "confirm"
    :> Post '[JSON] NoContent

type CancelFileUpload =
  Summary "Cancel a file upload"
    :> Capture "file_id" FileId
    :> ReqBody '[JSON] UploadCancellationForm
    :> Post '[JSON] NoContent

type DeleteFile =
  Summary "Delete a file"
    :> Capture "file_id" FileId
    :> "delete"
    :> Delete '[JSON] NoContent

data FileRoutes mode = FileRoutes
  { register :: mode :- RegisterFile
  , confirm :: mode :- ConfirmFileUpload
  , cancel :: mode :- CancelFileUpload
  , delete :: mode :- DeleteFile
  }
  deriving stock (Generic)
