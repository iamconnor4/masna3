module Masna3.Server.Guards where

import Data.Aeson
import Effectful
import Effectful.Error.Static qualified as Error
import Effectful.Log qualified as Log
import Masna3.Api.File.FileId (FileId)
import Masna3.Api.Owner.OwnerId (OwnerId)
import Masna3.Api.Process.ProcessId (ProcessId)

import Masna3.Server.Database
import Masna3.Server.Effects
import Masna3.Server.Error
import Masna3.Server.Model.File.Query
import Masna3.Server.Model.File.Types
import Masna3.Server.Model.Owner.Query
import Masna3.Server.Model.Owner.Types
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

guardThatOwnerExists :: OwnerId -> Eff RouteEffects Owner
guardThatOwnerExists ownerId = do
  maybeOwner <- withPool (getOwnerById ownerId)
  case maybeOwner of
    Nothing ->
      Log.localData ["owner_id" .= ownerId] $
        Error.throwError (OwnerNotFoundError (OwnerNotFound ownerId))
    Just owner -> pure owner

guardThatFileExists :: FileId -> Eff RouteEffects File
guardThatFileExists fileId = do
  maybeFile <- withPool (getFileById fileId)
  case maybeFile of
    Nothing ->
      Log.localData ["file_id" .= fileId] $
        Error.throwError (FileNotFoundError (FileNotFound fileId))
    Just file -> pure file
