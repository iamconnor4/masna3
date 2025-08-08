module Masna3.Test.File where

import Test.Tasty
import Test.Tasty.HUnit

import Masna3.Test.Utils

spec :: TestEff TestTree
spec = testThese "File tests"
  [ testThis "Register file" testRegisterFile

  ]

testRegisterFile :: TestEff ()
testRegisterFile = undefined
