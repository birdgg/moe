module MoeWeb.Types where

import Effectful
import Effectful.Concurrent (Concurrent)
import Effectful.Error.Static (Error)
import Effectful.Log (Log)
import Effectful.Reader.Static qualified as ER
import Moe.Environment
import Servant (ServerError)

type MoeEff = Eff RouteEffects

type RouteEffects =
  '[ Error ServerError
   , Log
   , Concurrent
   , ER.Reader MoeEnv
   , IOE
   ]