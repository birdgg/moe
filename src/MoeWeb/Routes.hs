module MoeWeb.Routes where

import MoeWeb.API.Routes qualified as API
import Servant.API

type ServerRoutes = NamedRoutes Routes

data Routes mode = Routes
  { assets :: mode :- "static" :> Raw
  , api :: mode :- API.Routes
  }
  deriving stock (Generic)
