module Masna3.Server.AWS.URL where

import Amazonka.Presign
import Amazonka.S3.GetObject
import Amazonka.S3.Internal
import Data.ByteString
import Effectful
import Effectful.Reader.Static (Reader)
import Effectful.Reader.Static qualified as Reader
import Effectful.Time (Time)
import Effectful.Time qualified as Time

import Masna3.Server.Environment

newGetURL
  :: (Reader Masna3Env :> es, Time :> es)
  => BucketName
  -> Text
  -> Eff es StrictByteString
newGetURL bucketName path = do
  Masna3Env{s3AuthEnv} <- Reader.ask
  timestamp <- Time.currentTime
  pure $
    presignURL
      s3AuthEnv
      NorthVirginia
      timestamp
      (60 * 5) -- 5 minutes
      ( newGetObject
          bucketName
          (ObjectKey path)
      )

newPutURL bucketName mimetype path = do
  Masna3Env{s3AuthEnv} <- Reader.ask
  timestamp <- Time.currentTime
  pure $
    presignURL
      s3AuthEnv
      NorthVirginia
      timestamp
      (60 * 5) -- 5 minutes
      (
        newPutObject
          bucketName
          (ObjectKey path)
          (
            toBody $ "Content-Type=" <> mimetype
          )
      )