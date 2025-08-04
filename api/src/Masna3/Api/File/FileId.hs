module Masna3.Api.File.FileId where

import Data.UUID.Types
import Servant.API

newtype FileId = FileId UUID
  deriving stock (Generic)
  deriving
    (Eq, FromField, FromHttpApiData, FromJSON, Ord, Show, ToField, ToJSON)
    via UUID

newFileId :: MonadIO m => m FileId
newFileId = FileId <$> Heptapod.generate
