module MoeWeb.API.Server.Bangumi where

import Moe.Model.Bangumi.Types
import MoeWeb.API.Routes.Bangumi qualified as Bangumi
import MoeWeb.Types
import Servant hiding ((:>))

bangumiServer :: ServerT Bangumi.API MoeEff
bangumiServer =
  Bangumi.API'
    { getBangumi = pure $ Bangumi{bangumiId = 1, title = "Example Bangumi"}
    }
