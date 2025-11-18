module Masna3.Server.Jobs.Types where

import Data.Aeson
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.Newtypes
import Database.PostgreSQL.Simple.ToField
import Deriving.Aeson
import Effectful
import Effectful.Log (Log)
import Effectful.Log qualified as Log
import Effectful.PostgreSQL.Connection
import Effectful.Time

import Masna3.Server.Model.File.Query qualified as Query

data Masna3Job
  = ListExpiredFiles
  deriving stock (Eq, Generic, Ord, Show)
  deriving
    (FromJSON, ToJSON)
    via (CustomJSON '[FieldLabelModifier '[CamelToSnake], SumObjectWithSingleField, TagSingleConstructors])
          Masna3Job
  deriving
    (FromField, ToField)
    via Aeson Masna3Job

processJob
  :: ( IOE :> es
     , Log :> es
     , Time :> es
     , WithConnection :> es
     )
  => Masna3Job
  -> Eff es ()
processJob = \case
  ListExpiredFiles -> do
    files <- Query.listExpiredFiles
    Log.logInfo "Expired files" $
      object ["amount" .= length files]
