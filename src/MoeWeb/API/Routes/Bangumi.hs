{- HLINT ignore "Use newtype instead of data" -}
module MoeWeb.API.Routes.Bangumi where

import Moe.Model.Bangumi.Types
import Servant

type API = NamedRoutes API'

data API' mode = API'
  { getBangumi :: mode :- Get '[JSON] Bangumi
  }
  deriving stock (Generic)
