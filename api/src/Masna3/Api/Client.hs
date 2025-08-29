module Masna3.Api.Client
  ( registerFile
  ) where

import Data.Proxy
import Servant.API
import Servant.Client

import Masna3.Api
import Masna3.Api.File

masna3Client :: ServerRoutes (AsClientT ClientM)
masna3Client = client (Proxy @(NamedRoutes ServerRoutes))

registerFile :: FileRegistrationForm -> ClientM FileRegistrationResult
registerFile form =
  masna3Client
    // (.api)
    // (.files)
    // (.register)
    /: form
