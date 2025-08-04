module Masna3.Api.Owner.OwnerId where

import Control.Monad.IO.Class
import Data.Aeson
import Data.Text.Display
import Data.UUID.Types
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import GHC.Generics
import Heptapod qualified
import Servant.API

newtype OwnerId = OwnerId UUID
  deriving stock (Generic)
  deriving
    (Display)
    via ShowInstance UUID
  deriving
    (Eq, FromField, FromHttpApiData, FromJSON, Ord, Show, ToField, ToJSON)
    via UUID

newOwnerId :: MonadIO m => m OwnerId
newOwnerId = OwnerId <$> Heptapod.generate
