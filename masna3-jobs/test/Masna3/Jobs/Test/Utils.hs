module Masna3.Jobs.Test.Utils where

import Data.Pool (Pool)
import Data.Text qualified as Text
import Database.PostgreSQL.Simple qualified as PG
import Effectful
import Effectful.Log (Log)
import Effectful.Log qualified as Log
import Effectful.PostgreSQL (WithConnection)
import Effectful.PostgreSQL qualified as DB
import Effectful.Reader.Static (Reader)
import Effectful.Reader.Static qualified as Reader
import Effectful.Time
import Effectful.Time qualified as Time
import Log.Backend.StandardOutput qualified as Log
import Test.Tasty (TestTree)
import Test.Tasty.HUnit qualified as Test

type TestEff a =
  Eff
    '[ Time
     , Reader TestEnv
     , Log
     , IOE
     ]
    a

data TestEnv = TestEnv
  { pool :: Pool PG.Connection
  }

testThis :: TestEnv -> Text -> TestEff () -> TestTree
testThis env name assertion = Test.testCase (Text.unpack name) $ do
  runEff $ do
    Log.withStdOutLogger $ \logger -> do
      assertion
        & Time.runTime
        & Reader.runReader env
        & Log.runLog "masna3-jobs-test" logger Log.defaultLogLevel

withTestPool
  :: forall a es
   . (IOE :> es, Reader TestEnv :> es)
  => Eff (WithConnection ': es) a -> Eff es a
withTestPool action = do
  TestEnv{pool} <- Reader.ask
  DB.runWithConnectionPool pool $ do
    DB.withTransaction action
