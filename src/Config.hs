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
  deriving (Show, Generic, ToJSON, FromJSON)

data ServiceConfigPreferences = ServiceConfigPreferences
  { commitMessage :: ServiceConfigCommitMessage,
    push :: ServiceConfigPush,
    add :: ServiceConfigAdd
  }
  deriving (Show, Generic, ToJSON, FromJSON)

data ServiceConfigCommitMessage = ServiceConfigCommitMessage
  { includeDate :: Bool,
    messageBasedOnChanges :: Bool,
    defaultMessage :: String,
    argsForCommitCommand :: [String]
  }
  deriving (Show, Generic, ToJSON, FromJSON)

data ServiceConfigAdd = ServiceConfigAdd
  { addAllPreCommit :: Bool,
    argsForAddCommand :: [String]
  }
  deriving (Show, Generic, ToJSON, FromJSON)

data ServiceConfigPush = ServiceConfigPush
  { enablePushCommand :: Bool,
    argsForPushCommand :: [String]
  }
  deriving (Show, Generic, ToJSON, FromJSON)

getConfig :: IO (Maybe ServiceConfig)
getConfig = do
  Data.Yaml.decodeFileThrow ".gitautosync.yaml" :: IO (Maybe ServiceConfig)