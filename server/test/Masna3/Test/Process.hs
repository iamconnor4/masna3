module Masna3.Test.Process where

import Masna3.Api.Client qualified as Client
import Masna3.Api.File
import Masna3.Api.Process
import Test.Tasty

import Masna3.Server.Model.Owner.Types
import Masna3.Server.Model.Owner.Update qualified as Update
import Masna3.Test.Utils

spec :: TestEnv -> TestTree
spec env =
  testGroup
    "Process tests"
    [ testThis env "Register process" testRegisterProcess
    , testThis env "Confirm process" testConfirmProcessNoFiles
    , testThis env "Confirm process invalid transition" testConfirmProcessNoFilesInvalidTransition
    , testThis env "Cancel process" testCancelProcessNoFiles
    , testThis env "Cancel process invalid transition" testCancelProcessNoFilesInvalidTransition
    , testThis env "Confirm process with 1 file" testConfirmProcessWith1File
    , testThis env "Confirm process with 2 files" testConfirmProcessWith2Files
    , testThis env "Cancel process with 1 file" testCancelProcessWith1File
    , testThis env "Cancel process with 2 files" testCancelProcessWith2Files
    ]

testRegisterProcess :: TestEff ()
testRegisterProcess = do
  owner <- newOwner "test-client-proc-1"
  withTestPool $ Update.insertOwner owner
  let ownerId = owner.ownerId
  let form = ProcessRegistrationForm ownerId
  void $ assertRight "Register process" =<< runRequest (Client.registerProcess form)

testConfirmProcessNoFiles :: TestEff ()
testConfirmProcessNoFiles = do
  owner <- newOwner "test-client-proc-2"
  withTestPool $ Update.insertOwner owner
  let ownerId = owner.ownerId
  let form = ProcessRegistrationForm ownerId
  result <- assertRight "Register process" =<< runRequest (Client.registerProcess form)
  void $ assertRight "Confirm process" =<< runRequest (Client.confirmProcess result.processId)

testConfirmProcessNoFilesInvalidTransition :: TestEff ()
testConfirmProcessNoFilesInvalidTransition = do
  owner <- newOwner "test-client-proc-3"
  withTestPool $ Update.insertOwner owner
  let ownerId = owner.ownerId
  let form = ProcessRegistrationForm ownerId
  result <- assertRight "Register process" =<< runRequest (Client.registerProcess form)
  void $ assertRight "Confirm process" =<< runRequest (Client.confirmProcess result.processId)
  void $ assertLeftWithStatus "Confirm process" 404 =<< runRequest (Client.confirmProcess result.processId)

testCancelProcessNoFiles :: TestEff ()
testCancelProcessNoFiles = do
  owner <- newOwner "test-client-proc-4"
  withTestPool $ Update.insertOwner owner
  let ownerId = owner.ownerId
  let form = ProcessRegistrationForm ownerId
  result <- assertRight "Register process" =<< runRequest (Client.registerProcess form)
  void $ assertRight "Cancel process" =<< runRequest (Client.cancelProcess result.processId)

testCancelProcessNoFilesInvalidTransition :: TestEff ()
testCancelProcessNoFilesInvalidTransition = do
  owner <- newOwner "test-client-proc-5"
  withTestPool $ Update.insertOwner owner
  let ownerId = owner.ownerId
  let form = ProcessRegistrationForm ownerId
  result <- assertRight "Register process" =<< runRequest (Client.registerProcess form)
  void $ assertRight "Cancel process" =<< runRequest (Client.cancelProcess result.processId)
  void $ assertLeftWithStatus "Cancel process" 404 =<< runRequest (Client.cancelProcess result.processId)

testConfirmProcessWith1File :: TestEff ()
testConfirmProcessWith1File = do
  owner <- newOwner "test-client-proc-6"
  withTestPool $ Update.insertOwner owner
  let ownerId = owner.ownerId
  let processForm = ProcessRegistrationForm ownerId
  registerProcessResult <- assertRight "Register process" =<< runRequest (Client.registerProcess processForm)
  let fileName = "toto.txt"
      mimeType = "text/plain"
      processId = Just registerProcessResult.processId
  let fileForm = FileRegistrationForm fileName owner.ownerId mimeType processId
  registerFileResult <- assertRight "Register file" =<< runRequest (Client.registerFile fileForm)
  void $ assertRight "Confirm file" =<< runRequest (Client.confirmFile registerFileResult.fileId)
  void $ assertRight "Confirm process" =<< runRequest (Client.confirmProcess registerProcessResult.processId)

testConfirmProcessWith2Files :: TestEff ()
testConfirmProcessWith2Files = do
  owner <- newOwner "test-client-proc-7"
  withTestPool $ Update.insertOwner owner
  let ownerId = owner.ownerId
  let processForm = ProcessRegistrationForm ownerId
  registerProcessResult <- assertRight "Register process" =<< runRequest (Client.registerProcess processForm)
  let fileName = "toto.txt"
      mimeType = "text/plain"
      processId = Just registerProcessResult.processId
  let fileForm = FileRegistrationForm fileName owner.ownerId mimeType processId
  registerFileResult <- assertRight "Register file" =<< runRequest (Client.registerFile fileForm)
  registerFileResult2 <- assertRight "Register file" =<< runRequest (Client.registerFile fileForm)
  void $ assertRight "Confirm file" =<< runRequest (Client.confirmFile registerFileResult.fileId)
  void $ assertRight "Confirm file " =<< runRequest (Client.confirmFile registerFileResult2.fileId)
  void $ assertRight "Confirm process" =<< runRequest (Client.confirmProcess registerProcessResult.processId)

testCancelProcessWith1File :: TestEff ()
testCancelProcessWith1File = do
  owner <- newOwner "test-client-proc-8"
  withTestPool $ Update.insertOwner owner
  let ownerId = owner.ownerId
  let processForm = ProcessRegistrationForm ownerId
  registerProcessResult <- assertRight "Register process" =<< runRequest (Client.registerProcess processForm)
  let fileName = "toto.txt"
      mimeType = "text/plain"
      processId = Just registerProcessResult.processId
  let fileForm = FileRegistrationForm fileName owner.ownerId mimeType processId
  registerFileResult <- assertRight "Register file" =<< runRequest (Client.registerFile fileForm)
  void $ assertRight "Confirm file" =<< runRequest (Client.confirmFile registerFileResult.fileId)
  void $ assertRight "Cancel process" =<< runRequest (Client.cancelProcess registerProcessResult.processId)

testCancelProcessWith2Files :: TestEff ()
testCancelProcessWith2Files = do
  owner <- newOwner "test-client-proc-9"
  withTestPool $ Update.insertOwner owner
  let ownerId = owner.ownerId
  let processForm = ProcessRegistrationForm ownerId
  registerProcessResult <- assertRight "Register process" =<< runRequest (Client.registerProcess processForm)
  let fileName = "toto.txt"
      mimeType = "text/plain"
      processId = Just registerProcessResult.processId
  let fileForm = FileRegistrationForm fileName owner.ownerId mimeType processId
  registerFileResult <- assertRight "Register file" =<< runRequest (Client.registerFile fileForm)
  registerFileResult2 <- assertRight "Register file" =<< runRequest (Client.registerFile fileForm)
  void $ assertRight "Confirm file" =<< runRequest (Client.confirmFile registerFileResult.fileId)
  void $ assertRight "Confirm file " =<< runRequest (Client.confirmFile registerFileResult2.fileId)
  void $ assertRight "Cancel process" =<< runRequest (Client.cancelProcess registerProcessResult.processId)
