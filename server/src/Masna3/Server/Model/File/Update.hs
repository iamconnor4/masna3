module Masna3.Server.Model.File.Update where

import Database.PostgreSQL.Entity
import Effectful
import Effectful.PostgreSQL

import Masna3.Server.Model.File.Types

insertFile :: (IOE :> es, WithConnection :> es) => File -> Eff es ()
insertFile file = void $ execute (_insert @File) file
