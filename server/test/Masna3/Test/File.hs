module Masna3.Test.File where

import Data.Text.Display
import Data.Text.Encoding qualified as Text
import Effectful.Reader.Static qualified as Reader
import Test.Tasty

import Masna3.Server.AWS.URL
import Masna3.Server.Database
import Masna3.Server.Environment
import Masna3.Server.Model.File.Query qualified as Query
import Masna3.Server.Model.File.Types
import Masna3.Server.Model.File.Update qualified as Update
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
  Masna3Env{awsBucket} <- Reader.ask
  file <-
    newFile
      owner.ownerId
      fileName
      awsBucket
      mimeType
  let path = display owner.ownerId <> "/" <> display file.fileId <> "/" <> fileName
  withPool (Update.insertFile file)

  actualFile <- assertJust "There is a file" =<< withPool (Query.getFileById file.fileId)
  assertEqual
    "Files are the same"
    actualFile.fileId
    file.fileId
