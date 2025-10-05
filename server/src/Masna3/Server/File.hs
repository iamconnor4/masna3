module Masna3.Server.File where

import Data.Text.Encoding qualified as Text
import Effectful
import Effectful.Error.Static qualified as Error
import Effectful.Reader.Static qualified as Reader
import Effectful.Time qualified as Time
import Masna3.Api.File
import Masna3.Api.File.FileId
import Servant.API.ContentTypes

import Masna3.Server.AWS.URL
import Masna3.Server.Database
import Masna3.Server.Effects
import Masna3.Server.Environment
import Masna3.Server.Error (InvalidTransitionError (NotPendingToUploaded), Masna3Error (FileNotFound, InvalidTransition))
import Masna3.Server.Model.File.Query qualified as Query
import Masna3.Server.Model.File.Types
import Masna3.Server.Model.File.Update qualified as Update

registerHandler :: FileRegistrationForm -> Eff RouteEffects FileRegistrationResult
registerHandler form = do
  Masna3Env{awsBucket} <- Reader.ask
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

confirmHandler :: FileId -> Eff RouteEffects NoContent
confirmHandler fileId = do
  file <- guardThatFileExists fileId
  case file.status of
    Pending -> do
      timestamp <- Time.currentTime
      withPool (Update.confirmFile fileId timestamp)
      pure NoContent
    _ -> Error.throwError (InvalidTransition (NotPendingToUploaded fileId))

guardThatFileExists :: FileId -> Eff RouteEffects File
guardThatFileExists fileId = do
  maybeFile <- withPool (Query.getFileById fileId)
  case maybeFile of
    Nothing -> Error.throwError (FileNotFound fileId)
    Just file -> pure file

cancelHandler :: FileId -> UploadCancellationForm -> Eff es NoContent
cancelHandler _ _ = pure NoContent
