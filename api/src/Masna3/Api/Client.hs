module Masna3.Api.Client
  ( registerFile
  , confirmFile
  ) where

import Data.Proxy
import Servant.API
import Servant.Client

import Masna3.Api
import Masna3.Api.File
import Masna3.Api.File.FileId

masna3Client :: ServerRoutes (AsClientT ClientM)
masna3Client = client (Proxy @(NamedRoutes ServerRoutes))

registerFile :: FileRegistrationForm -> ClientM FileRegistrationResult
registerFile form =
  masna3Client
    // (.api)
    // (.files)
    // (.register)
    /: form

confirmFile :: FileId -> ClientM NoContent
confirmFile fileId =
  masna3Client
    // (.api)
    // (.files)
    // (.confirm)
    /: fileId
