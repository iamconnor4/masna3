module Masna3.Server.Effects where

import Effectful
import Effectful.Error.Static (Error)
import Effectful.Log (Log)
import Effectful.Reader.Static (Reader)
import Effectful.Time (Time)

import Masna3.Server.Environment
import Masna3.Server.Error

type RouteEffects =
  [ Error Masna3Error
  , Log
  , Time
  , Reader Masna3Env
  , IOE
  ]
