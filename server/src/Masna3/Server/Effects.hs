module Masna3.Server.Effects where

import Effectful
import Effectful.Error.Static (Error)
import Effectful.Log (Log)
import Effectful.Reader.Static (Reader)
import Effectful.Time (Time)
import Servant.Server

import Masna3.Server.Environment

type RouteEffects =
  [ Error ServerError
  , Log
  , Time
  , Reader Masna3Env
  , IOE
  ]
