module MoeWeb.Server where

-- import Control.Exception (bracket)
import Control.Monad.Except qualified as Except
import Effectful
import Effectful.Concurrent
import Effectful.Error.Static (runErrorWith)
import Effectful.Reader.Static qualified as ER
import Log (Logger, (.=))
import Log qualified
import Log.Backend.StandardOutput qualified as Log
import Network.Wai.Handler.Warp (
  defaultSettings,
  runSettings,
  setPort,
 )
import Servant.Server
import Servant.Server.Generic (AsServerT)

import Moe.Environment
import Moe.Logging qualified as Logging
import MoeWeb.API.Server qualified as API
import MoeWeb.Routes
import MoeWeb.Types
import Servant (serveDirectoryWebApp)

runMoe :: IO ()
runMoe = runEff $ do
  let env = MoeEnv{environment = Development, httpPort = 4000}
  let withLogger = Log.withJsonStdOutLogger
  withLogger (`runServer` env)

runServer :: (IOE :> es) => Logger -> MoeEnv -> Eff es ()
runServer appLogger moeEnv = do
  let server = mkServer appLogger moeEnv
  let warpSettings =
        setPort (fromIntegral moeEnv.httpPort) defaultSettings
  liftIO $
    runSettings
      warpSettings
      server

mkServer :: Logger -> MoeEnv -> Application
mkServer logger moeEnv =
  serveWithContextT
    (Proxy @ServerRoutes)
    EmptyContext
    (naturalTransform moeEnv logger)
    moeServer

moeServer ::
  Routes (AsServerT MoeEff)
moeServer =
  Routes
    { assets = serveDirectoryWebApp "./static"
    , api = API.apiServer
    }

naturalTransform ::
  MoeEnv ->
  Logger ->
  MoeEff a ->
  Handler a
naturalTransform moeEnv logger app = do
  result <-
    liftIO $
      Right
        <$> app
          -- & runDB floraEnv.pool
          & runErrorWith
            ( \callstack err -> do
                Log.logInfo "Server error" $
                  Log.object
                    [ "error_http_code" .= errHTTPCode err
                    , "error_reason_phrase" .= errReasonPhrase err
                    , "exception" .= prettyCallStack callstack
                    ]
                pure . Left $ err
            )
          & Logging.runLog moeEnv.environment logger
          & runConcurrent
          & ER.runReader moeEnv
          & runEff
  either Except.throwError pure result