module Masna3.Server where

import Data.Function ((&))
import Effectful
import Effectful.Error.Static
import Servant qualified as Servant
import Servant (ServerError, Handler, Application)

type RouteEffects =
  Eff '[Error ServerError]

runMasna3
  :: IOE :> es
  => (Application -> IO ())
  -> Eff es ()
runMasna3 runApp = do
  serveWithContextT
    routes
    (serviceAuthentication services :. EmptyContext)
    (handleRoute (logRunner misc . localData ["request_id" .= reqId]))
    masna3Server

handleRoute
  :: Eff RouteEffects a
  -> Handler a
handleRoute action = do
  err <- liftIO $ do
      Right action
        & handleErrorNoCallStackWith handleServerError
        & runEff
  eitehr Servant.throwError pure err

handleServerError :: ServerError -> Eff es (Either ServerError a)
handleServerError = pure . Left
