{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module Masna3.Server.Model.Owner.Query where

import Data.Text (Text)
import Database.PostgreSQL.Entity
import Database.PostgreSQL.Entity.Internal.QQ
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
  result <- query (_select @Owner <> _where [[field| owner_name |]]) (Only ownerName)
  case result of
    [] -> pure Nothing
    [r] -> pure (Just r)
    _ -> Error.throwError (TooManyRows ownerName)
