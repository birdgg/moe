module MoeWeb.API.Routes where

import MoeWeb.API.Routes.Bangumi qualified as BangumiAPI
import Servant

type Routes = "api" :> NamedRoutes Routes'

data Routes' mode = Routes'
  { bangumis :: mode :- "bangumis" :> BangumiAPI.API
  }
  deriving stock (Generic)
