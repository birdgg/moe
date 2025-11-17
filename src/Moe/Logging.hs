module Moe.Logging (
  runLog,
)
where

import Data.Text.Display (display)
import Effectful
import Effectful.Log (Log)
import Effectful.Log qualified as Log
import Log (Logger)

import Moe.Environment

-- | Wrapper around 'Log.runLogT' with necessary metadata
runLog ::
  forall (es :: [Effect]) (a :: Type).
  (IOE :> es) =>
  DeploymentEnv ->
  Logger ->
  Eff (Log : es) a ->
  Eff es a
runLog env logger logAction =
  Log.runLog ("moe-" <> suffix) logger Log.defaultLogLevel logAction
 where
  suffix = display env