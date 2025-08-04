module Masna3.Api.File.FileId where

import Control.Monad.IO.Class
import Data.Aeson
import Data.Text.Display
import Data.UUID.Types
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import GHC.Generics
import Heptapod qualified
import Servant.API

newtype FileId = FileId UUID
  deriving stock (Generic)
  deriving
    (Display)
    via ShowInstance UUID
  deriving
    (Eq, FromField, FromHttpApiData, FromJSON, Ord, Show, ToField, ToJSON)
    via UUID

newFileId :: MonadIO m => m FileId
newFileId = FileId <$> Heptapod.generate
