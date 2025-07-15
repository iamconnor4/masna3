module Masna3.Api.File.Types where

import Data.UUID.Types
import Servant.API

newtype FileId = FileId UUID
  deriving newtype (Eq, FromHttpApiData, Ord, Show)
