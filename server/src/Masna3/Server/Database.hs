module Masna3.Server.Database where

import Effectful
import Effectful.PostgreSQL (WithConnection)
import Effectful.PostgreSQL qualified as DB
import Effectful.Reader.Static (Reader)
import Effectful.Reader.Static qualified as Reader

import Masna3.Server.Environment

withPool
  :: forall a es
   . (IOE :> es, Reader Masna3Env :> es)
  => Eff (WithConnection ': es) a -> Eff es a
withPool action = do
  Masna3Env{pool} <- Reader.ask
  DB.runWithConnectionPool pool $
    DB.withTransaction action
