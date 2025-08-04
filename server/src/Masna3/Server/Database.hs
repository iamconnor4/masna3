module Masna3.Server.Database where

import Data.Int
import Data.Pool qualified as Pool
import Effectful
import Effectful.Dispatch.Static (unsafeEff_)
import Effectful.Error.Static (Error)
import Effectful.PostgreSQL (WithConnection)
import Effectful.PostgreSQL qualified as DB
import Effectful.PostgreSQL.Connection.Pool qualified as DB
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
