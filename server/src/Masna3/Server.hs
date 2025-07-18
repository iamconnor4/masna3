module Masna3.Server where

import Auth.Biscuit.Servant
import Data.Function ((&))
import Data.Proxy
import Effectful
import Effectful.Error.Static
import Effectful.Log (Log)
import Effectful.Log qualified as Log
import Effectful.Time
import Log (Logger)
import Masna3.Api
import Masna3.Api.File
import Network.Wai.Handler.Warp
import Network.Wai.Log qualified as WaiLog
import Servant qualified
import Servant.API (NamedRoutes)
import Servant.Server
import Servant.Server.Generic

import Masna3.Server.Environment
import Masna3.Server.File

type RouteEffects =
  [ Error ServerError
  , Log
  , Time
  , IOE
  ]

runMasna3
  :: IOE :> es
  => Logger
  -> Masna3Env
  -> Eff es ()
runMasna3 logger environment = do
  loggingMiddleware <- Log.runLog "masna3-server" logger Log.defaultLogLevel WaiLog.mkLogMiddleware
  let server = makeServer logger environment
  let warpSettings =
        setPort (fromIntegral environment.httpPort) defaultSettings
  liftIO
    $ runSettings warpSettings
    $ loggingMiddleware
      . const
    $ server

makeServer
  :: Logger
  -> Masna3Env
  -> Application
makeServer logger environment =
  serveWithContextT
    (Proxy @(NamedRoutes ServerRoutes))
    (genBiscuitCtx environment.publicKey)
    (handleRoute logger)
    masna3Server

handleRoute
  :: Logger
  -> Eff RouteEffects a
  -> Handler a
handleRoute logger action = do
  err <-
    liftIO $
      Right <$> action
        & runErrorNoCallStackWith handleServerError
        & Log.runLog "masna3-server" logger Log.defaultLogLevel
        & runTime
        & runEff
  either Servant.throwError pure err

masna3Server :: ServerRoutes (AsServerT (Eff RouteEffects))
masna3Server =
  ServerRoutes
    { api = apiServer
    , documentation = undefined
    }

apiServer :: ServerT (NamedRoutes APIRoutes) (Eff RouteEffects)
apiServer =
  APIRoutes
    { file = fileServer
    }

fileServer :: ServerT (NamedRoutes FileRoutes) (Eff RouteEffects)
fileServer =
  FileRoutes
    { register = registerHandler
    , confirm = confirmHandler
    , cancel = cancelHandler
    }

handleServerError :: ServerError -> Eff es (Either ServerError a)
handleServerError = pure . Left
