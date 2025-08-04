{-# LANGUAGE QuasiQuotes #-}

module Masna3.Server.Model.Owner.Query where

import Data.Text (Text)
import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Simple.Types
import Effectful
import Effectful.Error.Static (Error)
import Effectful.Error.Static qualified as Error
import Effectful.PostgreSQL

import Masna3.Server.Error
import Masna3.Server.Model.Owner.Types

getOwnerByName
  :: ( Error Masna3Error :> es
     , IOE :> es
     , WithConnection :> es
     )
  => Text -> Eff es (Maybe Owner)
getOwnerByName ownerName = do
  result <- query q (Only ownerName)
  case result of
    [] -> pure Nothing
    [r] -> pure (Just r)
    _ -> Error.throwError (TooManyRows ownerName)
  where
    q =
      [sql|
    SELECT o1.owner_id
         , o1.owner_name
         , o1.created_at
         , o1.updated_at
    FROM owners as o1
    WHERE o1.owner_name = ?
    |]
