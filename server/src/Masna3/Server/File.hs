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
import Masna3.Api.Process.ProcessId (ProcessId)
import Servant.API.ContentTypes

import Masna3.Server.AWS.URL
import Masna3.Server.Database
import Masna3.Server.Effects
import Masna3.Server.Environment
import Masna3.Server.Error
import Masna3.Server.Model.File.Query qualified as Query
import Masna3.Server.Model.File.Types
import Masna3.Server.Model.File.Update qualified as Update
import Masna3.Server.Model.Owner.Query (getOwnerById)
import Masna3.Server.Model.Owner.Types (Owner)
import Masna3.Server.Model.Process.Query
import Masna3.Server.Model.Process.Types
import Masna3.Server.Model.ProcessFile.Types (newProcessFile)
import Masna3.Server.Model.ProcessFile.Update qualified as ProcessFileUpdate

guardThatProcessExists :: ProcessId -> Eff RouteEffects Process
guardThatProcessExists processId = do
  maybeProcess <- withPool (getProcessById processId)
  case maybeProcess of
    Nothing ->
      Log.localData ["process_id" .= processId] $
        Error.throwError (ProcessNotFoundError (ProcessNotFound processId))
    Just process -> pure process

registerHandler :: FileRegistrationForm -> Eff RouteEffects FileRegistrationResult
registerHandler form = do
  Masna3Env{awsBucket} <- Reader.ask
  guardThatOwnerExists form.ownerId
  void $ traverse guardThatProcessExists form.processId
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
  processFile <- traverse (`newProcessFile` file.fileId) form.processId
  withPool $ do
    Update.insertFile file
    traverse ProcessFileUpdate.insertProcessFile processFile
  pure FileRegistrationResult{fileId = file.fileId, url, processId = form.processId}

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
