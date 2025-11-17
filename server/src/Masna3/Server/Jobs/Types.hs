module Masna3.Server.Jobs.Types where

import Data.Aeson
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.Newtypes
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.Types
import Deriving.Aeson
import Effectful
import GHC.Generics

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

processJob :: Masna3Job -> Eff es ()
processJob = undefined
