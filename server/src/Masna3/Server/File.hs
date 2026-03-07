module Masna3.Server.File where

import Data.Aeson
import Data.Text.Encoding qualified as Text
import Effectful
import Effectful.Error.Static qualified as Error
import Effectful.Log qualified as Log
import Effectful.Reader.Static qualified as Reader
import Effectful.Time qualified as Time
import Masna3.Api.File
import Masna3.Api.File.FileId
import Masna3.Api.Owner.OwnerId (OwnerId)
import Servant.API.ContentTypes

import Masna3.Server.AWS.URL
import Masna3.Server.Database
import Masna3.Server.Effects
import Masna3.Server.Environment
import Masna3.Server.Error (FileNotFound (..), InvalidTransitionError (..), Masna3Error (..), MkInvalidTransitionFile (..), OwnerNotFound (..))
import Masna3.Server.Model.File.Query qualified as Query
import Masna3.Server.Model.File.Types
import Masna3.Server.Model.File.Update qualified as Update
import Masna3.Server.Model.Owner.Query (getOwnerById)
import Masna3.Server.Model.Owner.Types (Owner)

registerHandler :: FileRegistrationForm -> Eff RouteEffects FileRegistrationResult
registerHandler form = do
  Masna3Env{awsBucket} <- Reader.ask
  guardThatOwnerExists form.ownerId
  file <-
    newFile
      form.ownerId
      form.fileName
      awsBucket
      form.mimeType
  let path = display form.ownerId <> "/" <> display file.fileId <> "/" <> form.fileName
  url <-
    Text.decodeUtf8
      <$> newPutURL
        awsBucket
        form.mimeType
        path
  withPool (Update.insertFile file)
  pure FileRegistrationResult{fileId = file.fileId, url}

guardThatOwnerExists :: OwnerId -> Eff RouteEffects Owner
guardThatOwnerExists ownerId = do
  maybeOwner <- withPool (getOwnerById ownerId)
  case maybeOwner of
    Nothing ->
      Log.localData ["owner_id" .= ownerId] $
        Error.throwError (OwnerNotFoundError (OwnerNotFound ownerId))
    Just owner -> pure owner

confirmHandler :: FileId -> Eff RouteEffects NoContent
confirmHandler fileId = do
  file <- guardThatFileExists fileId
  case file.status of
    Pending -> do
      timestamp <- Time.currentTime
      withPool (Update.confirmFile fileId timestamp)
      pure NoContent
    _ ->
      Log.localData ["file_id" .= fileId] $
        Error.throwError (InvalidTransition (NotPendingToUploaded (MkInvalidTransitionFile fileId)))

guardThatFileExists :: FileId -> Eff RouteEffects File
guardThatFileExists fileId = do
  maybeFile <- withPool (Query.getFileById fileId)
  case maybeFile of
    Nothing ->
      Log.localData ["file_id" .= fileId] $
        Error.throwError (FileNotFoundError (FileNotFound fileId))
    Just file -> pure file

cancelHandler :: FileId -> UploadCancellationForm -> Eff es NoContent
cancelHandler _ _ = pure NoContent

deleteHandler :: FileId -> Eff RouteEffects NoContent
deleteHandler fileId = do
  void $ guardThatFileExists fileId
  withPool (Update.deleteFile fileId)
  pure NoContent
