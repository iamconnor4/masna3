{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module Masna3.Server.Model.File.Update where

import Data.Time
import Database.PostgreSQL.Entity
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Effectful
import Effectful.PostgreSQL
import Masna3.Api.File.FileId

import Masna3.Server.Model.File.Types

insertFile :: (IOE :> es, WithConnection :> es) => File -> Eff es ()
insertFile file = void $ execute (_insert @File) file

confirmFile :: (IOE :> es, WithConnection :> es) => FileId -> UTCTime -> Eff es ()
confirmFile fileId timestamp = void $ execute q (timestamp, fileId)
  where
    q =
      [sql|
        UPDATE files SET (status = "uploaded", uploaded_at = ?)
        WHERE file_id = ?; 
       |]
