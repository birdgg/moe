module Moe.Model.Bangumi.Types where

import Data.Aeson

-- | Minimal description of a Bangumi entry used by the API layer.
data Bangumi = Bangumi
  { bangumiId :: Int
  , title :: Text
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON Bangumi