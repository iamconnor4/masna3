module Masna3.Server.Process.Guards where

import Data.Aeson
import Effectful
import Effectful.Error.Static qualified as Error
import Effectful.Log qualified as Log
import Masna3.Api.Process.ProcessId (ProcessId)

import Masna3.Server.Database
import Masna3.Server.Effects
import Masna3.Server.Error
import Masna3.Server.Model.Process.Query
import Masna3.Server.Model.Process.Types

guardThatProcessExists :: ProcessId -> Eff RouteEffects Process
guardThatProcessExists processId = do
  maybeProcess <- withPool (getProcessById processId)
  case maybeProcess of
    Nothing ->
      Log.localData ["process_id" .= processId] $
        Error.throwError (ProcessNotFoundError (ProcessNotFound processId))
    Just process -> pure process

guardThatProcessFilesConfirmed :: ProcessId -> Eff RouteEffects ()
guardThatProcessFilesConfirmed processId = do
  unconfirmedFiles <- withPool (hasUnconfirmedFiles processId)
  case unconfirmedFiles of
    True ->
      Log.localData ["process_id" .= processId] $
        Error.throwError (ProcessFilesNotCompletedError (ProcessFilesNotCompleted processId))
    False -> pure ()
