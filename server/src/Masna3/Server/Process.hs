module Masna3.Server.Process where

import Effectful
import Masna3.Api.Process
import Masna3.Api.Process.ProcessId (ProcessId)
import Servant.API.ContentTypes

import Masna3.Server.Database
import Masna3.Server.Effects
import Masna3.Server.Guards
import Masna3.Server.Model.Process.Types
import Masna3.Server.Model.Process.Update qualified as Update

registerHandler :: ProcessRegistrationForm -> Eff RouteEffects ProcessRegistrationResult
registerHandler form = do
  guardThatOwnerExists form.ownerId
  process <-
    newProcess
      form.ownerId
  withPool (Update.insertProcess process)
  pure ProcessRegistrationResult{processId = process.processId}

completeHandler :: ProcessId -> Eff RouteEffects NoContent
completeHandler processId = do
  void $ guardThatProcessExists processId
  void $ guardThatProcessFilesConfirmed processId
  withPool (Update.deleteProcess processId)
  pure NoContent

cancelHandler :: ProcessId -> Eff RouteEffects NoContent
cancelHandler processId = do
  void $ guardThatProcessExists processId
  withPool (Update.deleteProcessAndFiles processId)
  pure NoContent
