module Masna3.Test.File where

import Masna3.Api.Client qualified as Client
import Masna3.Api.File
import Test.Tasty

import Masna3.Server.Database
import Masna3.Server.Model.Owner.Types
import Masna3.Server.Model.Owner.Update qualified as Update
import Masna3.Test.Utils

spec :: TestEff TestTree
spec =
  testThese
    "File tests"
    [ testThis "Register file" testRegisterFile
    ]

testRegisterFile :: TestEff ()
testRegisterFile = do
  owner <- newOwner "test-client"
  withPool $ Update.insertOwner owner
  let fileName = "toto.txt"
      mimeType = "text/plain"
  let form = FileRegistrationForm fileName owner.ownerId mimeType
  void $ assertRight "Register file" =<< runRequest (Client.registerFile form)
