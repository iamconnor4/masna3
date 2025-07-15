module Masna3.Api.File where

import Servant.API

import Masna3.Api.Types.File

type RegisterFile =
  Summary "Register a file for upload"
    :> "register"
    :> ReqBody '[JSON] FileRegistrationForm
    :> Post '[JSON] FileRegistrationResult

type ConfirmFileUpload =
  Summary "Register a file for upload"
    :> Capture "file_id"
    :> ReqBody '[JSON] UploadConfirmationForm
    :> Post '[JSON] ()

type CancelFileUpload =
  Summary "Cancel a file upload"
    :> Capture "file_id"
    :> ReqBody '[JSON] UploadCancellationForm
    :> Post '[JSON] ()
