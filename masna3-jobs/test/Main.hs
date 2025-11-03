module Main where

import Data.Aeson
import Data.ByteString
import Data.ByteString.Char8 qualified as C8
import Data.Pool (Pool)
import Data.Pool qualified as Pool
import Database.PostgreSQL.Simple qualified as PG
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.Newtypes
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.Types
import Deriving.Aeson
import Effectful
import Effectful.PostgreSQL (WithConnection)
import Effectful.PostgreSQL qualified as DB
import Effectful.Reader.Static qualified as Reader
import System.Environment
import System.Exit
import Test.Tasty

import Masna3.Jobs.Job (Job (..))
import Masna3.Jobs.Job qualified as Job
import Masna3.Jobs.Queue qualified as Queue
import Masna3.Jobs.Test.Utils

data JobPayload
  = PrintMessage Text
  | PurgeOrphanFiles
  deriving stock (Eq, Generic, Ord, Show)
  deriving
    (FromJSON, ToJSON)
    via (CustomJSON '[FieldLabelModifier '[CamelToSnake], SumObjectWithSingleField])
          JobPayload
  deriving
    (FromField, ToField)
    via Aeson JobPayload

mkPool
  :: IOE :> es
  => StrictByteString -- Database access information
  -> Eff es (Pool PG.Connection)
mkPool connectionInfo =
  liftIO $
    Pool.newPool $
      Pool.defaultPoolConfig
        (PG.connectPostgreSQL connectionInfo)
        PG.close
        30
        30

main :: IO ()
main = do
  connString <-
    lookupEnv "TEST_CONNSTRING" >>= \case
      Just c -> pure $ C8.pack c
      _ -> do
        putStrLn "Pass a PostgreSQL connstring to the test suite in the $TEST_CONNSTRING environment variable."
        putStrLn "Exiting…"
        exitFailure

  pool <- runEff $ mkPool connString
  let env = TestEnv pool
  runEff . Reader.runReader env $ withTestPool cleanUpQueues
  defaultMain $ testGroup "masna3-jobs tests" (specs env)

specs :: TestEnv -> [TestTree]
specs env =
  [ testThis
      env
      "Create new job"
      testCreateNewJob
  ]

testCreateNewJob :: TestEff ()
testCreateNewJob = do
  mJob <- withTestPool $ do
    Queue.createQueue "testqueue"
    Job.insertJob "testQueue" (PrintMessage "salam")
    Job.readJob "testQueue"

  job <- assertJust "Get a job from the queue" mJob
  result :: JobPayload <- assertSuccess "Payload decoded" (fromJSON job.message)
  assertEqual
    "Message payload is correct"
    result
    (PrintMessage "salam")

cleanUpQueues :: (IOE :> es, WithConnection :> es) => Eff es ()
cleanUpQueues = do
  _ :: [Only Int] <- DB.query_ "select * from pgmq.purge_queue('testqueue')"
  pure ()
