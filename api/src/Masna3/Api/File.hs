module Masna3.Api.File where

import Data.Aeson
import Data.FileId.Types
import Data.Text
import GHC.Generics
import Masna3.Api.File.OwnerId
import Servant.API

import Masna3.Api.File.FileId

data FileRegistrationForm = FileRegistrationForm
  { fileName :: Text
  , owner :: OwnerId
  , mimetype :: Text
  }
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (FromJSON, ToJSON)

data FileRegistrationResult = FileRegistrationResult
  { fileId :: FileId
  , url :: Text
  }
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (FromJSON, ToJSON)

data UploadConfirmationForm = UploadConfirmationForm
  { fileId :: FileId
  }
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (FromJSON, ToJSON)

data UploadCancellationForm = UploadCancellationForm
  { fileId :: FileId
  }
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (FromJSON, ToJSON)

type RegisterFile =
  Summary "Register a file for upload"
    :> "register"
    :> ReqBody '[JSON] FileRegistrationForm
    :> Post '[JSON] FileRegistrationResult

type ConfirmFileUpload =
  Summary "Register a file for upload"
    :> Capture "file_id" FileId
    :> ReqBody '[JSON] UploadConfirmationForm
    :> Post '[JSON] NoContent

type CancelFileUpload =
  Summary "Cancel a file upload"
    :> Capture "file_id" FileId
    :> ReqBody '[JSON] UploadCancellationForm
    :> Post '[JSON] NoContent

data FileRoutes mode = FileRoutes
  { register :: mode :- RegisterFile
  , confirm :: mode :- ConfirmFileUpload
  , cancel :: mode :- CancelFileUpload
  }
  deriving stock (Generic)
