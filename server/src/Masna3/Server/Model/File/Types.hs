module Masna3.Server.Model.File.Types where

import Data.ByteString (StrictByteString)
import Data.ByteString.Char8 qualified as BS8
import Data.Text (Text)
import Data.Text.Display
import Data.Time (UTCTime)
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import Effectful
import Effectful.Time (Time)
import Effectful.Time qualified as Time
import GHC.Generics
import Masna3.Api.File.FileId
import Masna3.Api.Owner.OwnerId

data FileStatus
  = Pending
  | Uploaded
  deriving stock (Eq, Generic, Ord, Show)

instance ToField FileStatus where
  toField Pending = Escape "pending"
  toField Uploaded = Escape "uploaded"

instance FromField FileStatus where
  fromField :: Field -> Maybe StrictByteString -> Conversion FileStatus
  fromField f mdata =
    case mdata of
      Nothing -> returnError UnexpectedNull f ""
      Just bs ->
        case bs of
          "pending" -> pure Pending
          "uploaded" -> pure Uploaded
          e ->
            returnError ConversionFailed f $
              "Conversion error: Expected valid FileStatus for 'status', got: " <> BS8.unpack e

data File = File
  { fileId :: FileId
  , ownerId :: OwnerId
  , filename :: Text
  , path :: Text
  , status :: FileStatus
  , bucket :: Text
  , mimetype :: Text
  , createdAt :: UTCTime
  , updatedAt :: Maybe UTCTime
  , uploadedAt :: Maybe UTCTime
  }

newFile
  :: (IOE :> es, Time :> es)
  => OwnerId
  -- ^ Owner
  -> Text
  -- ^ File name
  -> Text
  -- ^ Bucket
  -> Text
  -- ^ MIME type
  -> Eff es File
newFile ownerId filename bucket mimetype = do
  fileId <- newFileId
  createdAt <- Time.currentTime
  let path = display ownerId <> "/" <> display fileId <> "/" <> filename
  pure
    File
      { fileId
      , ownerId
      , filename
      , path
      , status = Pending
      , bucket
      , mimetype
      , createdAt
      , updatedAt = Nothing
      , uploadedAt = Nothing
      }
