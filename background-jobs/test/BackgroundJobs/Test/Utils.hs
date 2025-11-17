module BackgroundJobs.Test.Utils where

import Data.Aeson
import Data.Pool (Pool)
import Data.Text qualified as Text
import Database.PostgreSQL.Simple qualified as PG
import Effectful
import Effectful.Concurrent
import Effectful.Concurrent qualified as Concurrent
import Effectful.Log (Log)
import Effectful.Log qualified as Log
import Effectful.PostgreSQL (WithConnection)
import Effectful.PostgreSQL qualified as DB
import Effectful.Reader.Static (Reader)
import Effectful.Reader.Static qualified as Reader
import Effectful.Time
import Effectful.Time qualified as Time
import GHC.Stack
import Log.Backend.StandardOutput qualified as Log
import Test.Tasty (TestTree)
import Test.Tasty.HUnit qualified as Test

type TestM a = TestEff a

type TestEff =
  Eff
    '[ Time
     , Reader TestEnv
     , Log
     , Concurrent
     , IOE
     ]

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
        & Log.runLog "background-jobs-test" logger Log.defaultLogLevel
        & Concurrent.runConcurrent

withTestPool
  :: forall a es
   . (IOE :> es, Reader TestEnv :> es)
  => Eff (WithConnection ': es) a -> Eff es a
withTestPool action = do
  TestEnv{pool} <- Reader.ask
  DB.runWithConnectionPool pool $ do
    DB.withTransaction action

assertEqual :: (Eq a, HasCallStack, IOE :> es, Show a) => String -> a -> a -> Eff es ()
assertEqual message expected actual = liftIO $ Test.assertEqual message expected actual

assertJust :: (HasCallStack, IOE :> es) => String -> Maybe a -> Eff es a
assertJust _ (Just a) = pure a
assertJust message Nothing = liftIO $ Test.assertFailure message

assertRight :: (HasCallStack, IOE :> es) => String -> Either a b -> Eff es b
assertRight _ (Right b) = pure b
assertRight message (Left _a) = liftIO $ Test.assertFailure message

assertSuccess :: (HasCallStack, IOE :> es) => String -> Result b -> Eff es b
assertSuccess message result = assertRight message (toEither result)
  where
    toEither (Error m) = Left m
    toEither (Success a) = Right a
