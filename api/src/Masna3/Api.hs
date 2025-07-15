module Masna3.Api where

import GHC.Generics
import Servant.API

import Masna3.Api.File

data ServerRoutes mode = ServerRoutes
  { api :: mode :- "api" :> NamedRoutes APIRoutes
  , documentation :: mode :- "documentation" :> Raw
  }
  deriving (Generic)

data APIRoutes mode = APIRoutes
  { file :: mode :- "file" :> NamedRoutes FileRoutes
  }
  deriving (Generic)
