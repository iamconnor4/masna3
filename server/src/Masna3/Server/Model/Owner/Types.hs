module Masna3.Server.Model.Owner.Types where

import Data.Text (Text)
import Data.Time (UTCTime)
import Data.UUID.Types
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToRow
import Effectful
import Effectful.Time (Time)
import Effectful.Time qualified as Time
import GHC.Generics
import Heptapod qualified
import Masna3.Api.Owner.OwnerId

data Owner = Owner
  { ownerId :: OwnerId
  , ownerName :: Text
  , createdAt :: UTCTime
  , updatedAt :: Maybe UTCTime
  }
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (FromRow, ToRow)

newOwner :: (IOE :> es, Time :> es) => Text -> Eff es Owner
newOwner ownerName = do
  ownerId <- liftIO newOwnerId
  createdAt <- Time.currentTime
  let updatedAt = Nothing
  pure Owner{..}
