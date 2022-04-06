{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Config where

import qualified Data.ByteString
import qualified Data.Text
import Data.Text.Encoding (encodeUtf8)
import Data.Yaml (FromJSON, ToJSON, decodeFileThrow)
import GHC.Generics (Generic)

newtype ServiceConfig = ServiceConfig
  { servicePreferences :: ServiceConfigPreferences
  }
  deriving (Show, Generic, ToJSON, FromJSON)

data ServiceConfigPreferences = ServiceConfigPreferences
  { commitPreferences :: ServiceConfigCommitPreferences,
    pushPreferences :: ServiceConfigPushPreferences,
    addPreferences :: ServiceConfigAddPreferences
  }
  deriving (Show, Generic, ToJSON, FromJSON)

data ServiceConfigCommitPreferences = ServiceConfigCommitMessage
  { includeDateInCommitMessage :: Bool,
    defaultCommitMessage :: String,
    argsForCommitAction :: [String]
  }
  deriving (Show, Generic, ToJSON, FromJSON)

data ServiceConfigAddPreferences = ServiceConfigAdd
  { addAllBeforeCommitting :: Bool,
    argsForAddAction :: [String]
  }
  deriving (Show, Generic, ToJSON, FromJSON)

data ServiceConfigPushPreferences = ServiceConfigPush
  { pushToRemoteAfterCommit :: Bool,
    argsForPushAction :: [String]
  }
  deriving (Show, Generic, ToJSON, FromJSON)

getConfig :: IO (Maybe ServiceConfig)
getConfig = do
  Data.Yaml.decodeFileThrow ".gitautosync.yaml" :: IO (Maybe ServiceConfig)