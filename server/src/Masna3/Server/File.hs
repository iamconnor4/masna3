module Masna3.Server.File where

import Effectful
import Masna3.Api.File
import Masna3.Api.File.Types
import Servant.API.ContentTypes

registerHandler :: FileRegistrationForm -> Eff es FileRegistrationResult
registerHandler = undefined

confirmHandler :: FileId -> UploadConfirmationForm -> Eff es NoContent
confirmHandler = undefined

cancelHandler :: FileId -> UploadCancellationForm -> Eff es NoContent
cancelHandler = undefined
