module Masna3.Server.Model.ProcessFile.Update where

import Database.PostgreSQL.Entity
import Effectful
import Effectful.PostgreSQL

import Masna3.Server.Model.ProcessFile.Types

insertProcessFile :: (IOE :> es, WithConnection :> es) => ProcessFile -> Eff es ()
insertProcessFile processFile = void $ execute (_insert @ProcessFile) processFile
