module Masna3.Test.File where

import Masna3.Api.Client qualified as Client
import Masna3.Api.File
import Test.Tasty

import Masna3.Server.Model.Owner.Types
import Masna3.Server.Model.Owner.Update qualified as Update
import Masna3.Test.Utils

spec :: TestEnv -> TestTree
spec env =
  testGroup
    "File tests"
    [ testThis env "Register file" testRegisterFile
    , testThis env "Confirm File" testConfirmFile
    , testThis env "Confirm File Invalid Transition" testConfirmFileInvalidTransition
    , testThis env "Delete File" testDeleteFile
    , testThis env "Delete File Invalid Transition" testDeleteFileInvalidTransition
    ]

testRegisterFile :: TestEff ()
testRegisterFile = do
  owner <- newOwner "test-client"
  withTestPool $ Update.insertOwner owner
  let fileName = "toto.txt"
      mimeType = "text/plain"
  let form = FileRegistrationForm fileName owner.ownerId mimeType
  void $ assertRight "Register file" =<< runRequest (Client.registerFile form)

testConfirmFile :: TestEff ()
testConfirmFile = do
  owner <- newOwner "test-client-2"
  withTestPool $ Update.insertOwner owner
  let fileName = "toto.txt"
      mimeType = "text/plain"
  let form = FileRegistrationForm fileName owner.ownerId mimeType
  result <- assertRight "Register file" =<< runRequest (Client.registerFile form)
  void $ assertRight "Confirm File" =<< runRequest (Client.confirmFile result.fileId)

testConfirmFileInvalidTransition :: TestEff ()
testConfirmFileInvalidTransition = do
  owner <- newOwner "test-client-3"
  withTestPool $ Update.insertOwner owner
  let fileName = "toto.txt"
      mimeType = "text/plain"
  let form = FileRegistrationForm fileName owner.ownerId mimeType
  result <- assertRight "Register file" =<< runRequest (Client.registerFile form)
  void $ assertRight "Confirm File" =<< runRequest (Client.confirmFile result.fileId)
  void $ assertLeftWithStatus "Confirm File" 500 =<< runRequest (Client.confirmFile result.fileId)

testDeleteFile :: TestEff ()
testDeleteFile = do
  owner <- newOwner "test-client-4"
  withTestPool $ Update.insertOwner owner
  let fileName = "toto.txt"
      mimeType = "text/plain"
  let form = FileRegistrationForm fileName owner.ownerId mimeType
  result <- assertRight "Register file" =<< runRequest (Client.registerFile form)
  void $ assertRight "Confirm File" =<< runRequest (Client.confirmFile result.fileId)
  void $ assertRight "Delete File" =<< runRequest (Client.deleteFile result.fileId)

testDeleteFileInvalidTransition :: TestEff ()
testDeleteFileInvalidTransition = do
  owner <- newOwner "test-client-5"
  withTestPool $ Update.insertOwner owner
  let fileName = "toto.txt"
      mimeType = "text/plain"
  let form = FileRegistrationForm fileName owner.ownerId mimeType
  result <- assertRight "Register file" =<< runRequest (Client.registerFile form)
  void $ assertRight "Confirm File" =<< runRequest (Client.confirmFile result.fileId)
  void $ assertRight "Delete File" =<< runRequest (Client.deleteFile result.fileId)
  void $ assertLeftWithStatus "Delete File" 404 =<< runRequest (Client.deleteFile result.fileId)
