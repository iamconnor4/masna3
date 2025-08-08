module Masna3.Test.Utils where

import Data.Function ((&))
import Effectful
import Effectful.Error.Static (Error)
import Effectful.Error.Static qualified as Error
import Effectful.Reader.Static (Reader)
import Effectful.Reader.Static qualified as Reader
import Effectful.Time (Time)
import Effectful.Time qualified as Time
import GHC.Stack
import Test.Tasty (TestTree)
import Test.Tasty qualified as Test
import Test.Tasty.HUnit qualified as Test

import Masna3.Server.Environment
import Masna3.Server.Error

type TestEff a =
  Eff
    '[ Time
     , Error Masna3Error
     , Reader Masna3Env
     , IOE
     ]
    a

runTestEff :: TestEff a -> Masna3Env -> IO a
runTestEff action env = runEff $ do
  action
    & Time.runTime
    & Error.runErrorWith handleTestSuiteError
    & Reader.runReader env
  where
    handleTestSuiteError :: IOE :> es => CallStack -> Masna3Error -> Eff es a
    handleTestSuiteError callstack err = do
      liftIO $ putStrLn $ prettyCallStack callstack
      error (show err)

testThis :: HasCallStack => String -> TestEff () -> TestEff TestTree
testThis name assertion = do
  env <- Reader.ask @Masna3Env
  let test = runTestEff assertion env
  pure $ Test.testCase name test

testThese :: HasCallStack => String -> [TestEff TestTree] -> TestEff TestTree
testThese groupName tests = fmap (Test.testGroup groupName) newTests
  where
    newTests :: TestEff [TestTree]
    newTests = sequenceA tests

assertBool :: HasCallStack => String -> Bool -> TestEff ()
assertBool message assertion = liftIO $ Test.assertBool message assertion
