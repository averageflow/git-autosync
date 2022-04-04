{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Config where

import qualified Data.ByteString
import qualified Data.Text
import Data.Text.Encoding (encodeUtf8)
import Data.Yaml (FromJSON (parseJSON), ToJSON, Value (String), decodeFileThrow)
import Data.Yaml.Builder (ToYaml)
import Data.Yaml.Parser (FromYaml)
import GHC.Base (IO (IO))
import GHC.Generics

newtype ServiceConfig = ServiceConfig
  { preferences :: ServiceConfigPreferences
  }
  deriving (Show, Generic, ToYaml, FromYaml, ToJSON, FromJSON)

data ServiceConfigPreferences = ServiceConfigPreferences
  { commitMessage :: ServiceConfigCommitMessage,
    push :: ServiceConfigPush,
    add :: ServiceConfigAdd
  }
  deriving (Show, Generic, ToYaml, FromYaml, ToJSON, FromJSON)

data ServiceConfigCommitMessage = ServiceConfigCommitMessage
  { includeDate :: Bool,
    messageBasedOnChanges :: Bool,
    defaultMessage :: String,
    extraArgs :: [String]
  }
  deriving (Show, Generic, ToYaml, FromYaml, ToJSON, FromJSON)

newtype ServiceConfigAdd = ServiceConfigAdd
  { addAllPreCommit :: Bool
  }
  deriving (Show, Generic, ToYaml, FromYaml, ToJSON, FromJSON)

newtype ServiceConfigPush = ServiceConfigPush
  { enable :: Bool
  -- extraArgs :: [String]
  }
  deriving (Show, Generic, ToYaml, FromYaml, ToJSON, FromJSON)

getConfig = do
  parsedConfig <- Data.Yaml.decodeFileThrow ".gitautosync.yaml" :: IO (Maybe ServiceConfig)
  print parsedConfig