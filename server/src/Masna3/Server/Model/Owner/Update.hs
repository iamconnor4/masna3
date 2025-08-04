module Masna3.Server.Model.Owner.Update where

import Control.Monad (void)
import Database.PostgreSQL.Entity
import Effectful
import Effectful.PostgreSQL

import Masna3.Server.Model.Owner.Types

insertOwner
  :: ( IOE :> es
     , WithConnection :> es
     )
  => Owner -> Eff es ()
insertOwner owner = void $ execute (_insert @Owner) owner
