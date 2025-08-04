module Masna3.Api.Owner.OwnerId where

import Control.Monad.IO.Class
import Data.Aeson
import Data.UUID.Types
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import GHC.Generics
import Heptapod qualified

newtype OwnerId = OwnerId {getOwnerId :: UUID}
  deriving stock (Generic)
  deriving
    (Eq, FromField, FromJSON, Ord, Show, ToField, ToJSON)
    via UUID

newOwnerId :: MonadIO m => m OwnerId
newOwnerId = OwnerId <$> Heptapod.generate
