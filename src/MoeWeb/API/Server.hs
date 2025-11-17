module MoeWeb.API.Server where

import Servant

import MoeWeb.API.Routes (Routes' (bangumis))
import MoeWeb.API.Routes qualified as API
import MoeWeb.API.Server.Bangumi qualified as BangumiAPI
import MoeWeb.Types

apiServer :: ServerT API.Routes MoeEff
apiServer =
  API.Routes'
    { bangumis = BangumiAPI.bangumiServer
    }
