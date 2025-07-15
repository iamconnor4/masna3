module Masna3.Api.Types.File where

import Data.Aeson
import Data.Text
import Data.UUID.Types
import GHC.Generics

data FileRegistrationForm = FileRegistrationForm
  { fileName :: Text
  , owner :: Text
  , mimetype :: Text
  }
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (FromJSON, ToJSON)

data FileRegistrationResult = FileRegistrationResult
  { fileId :: UUID
  , url :: Text
  }
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (FromJSON, ToJSON)

data UploadConfirmationForm = UploadConfirmationForm
  { fileId :: UUID
  }
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (FromJSON, ToJSON)

data UploadCancellationForm = UploadCancellationForm
  { fileId :: UUID
  }
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (FromJSON, ToJSON)
