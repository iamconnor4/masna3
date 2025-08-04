{-# LANGUAGE QuasiQuotes #-}

module Masna3.Server.Model.Owner.Update where

import Control.Monad (void)
import Database.PostgreSQL.Simple.SqlQQ
import Effectful
import Effectful.PostgreSQL

import Masna3.Server.Model.Owner.Types

insertOwner
  :: ( IOE :> es
     , WithConnection :> es
     )
  => Owner -> Eff es ()
insertOwner Owner{..} = void $ execute q (ownerId, ownerName, createdAt, updatedAt)
  where
    q =
      [sql|
           INSERT INTO owners VALUES (
              ?
            , ?
            , ?
            , ?
            )
        |]
