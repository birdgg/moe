module Moe.Environment where

import Data.Text.Display

newtype Store = Store FilePath

data DeploymentEnv
  = Production
  | Development
  deriving stock (Bounded, Enum, Eq, Generic, Show)

instance Display DeploymentEnv where
  displayBuilder Production = "production"
  displayBuilder Development = "development"

data MoeEnv = MoeEnv
  { environment :: DeploymentEnv
  , httpPort :: Int
  }
  deriving stock (Generic, Show)