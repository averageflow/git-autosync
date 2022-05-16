{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module GitAutoSync.Config where

import qualified Data.ByteString
import qualified Data.Text
import Data.Text.Encoding (encodeUtf8)
import Data.Yaml (FromJSON, ToJSON, decodeFileThrow)
import GHC.Generics (Generic)

newtype ServicePreferences = ServicePreferences
  { managedObjects :: [ManagedObjectPreferences]
  }
  deriving (Show, Generic, ToJSON, FromJSON)

data ManagedObjectPreferences = ManagedObjectPreferences
  { location :: String,
    commitPreferences :: CommitPreferences,
    pushPreferences :: PushPreferences,
    addPreferences :: AddPreferences
  }
  deriving (Show, Generic, ToJSON, FromJSON)

data CommitPreferences = CommitPreferences
  { includeDateInCommitMessage :: Bool,
    defaultCommitMessage :: String,
    argsForCommitAction :: [String]
  }
  deriving (Show, Generic, ToJSON, FromJSON)

data AddPreferences = AddPreferences
  { addAllBeforeCommitting :: Bool,
    argsForAddAction :: [String]
  }
  deriving (Show, Generic, ToJSON, FromJSON)

data PushPreferences = PushPreferences
  { pushToRemoteAfterCommit :: Bool,
    argsForPushAction :: [String]
  }
  deriving (Show, Generic, ToJSON, FromJSON)

getConfig :: IO (Maybe ServicePreferences)
getConfig = do
  Data.Yaml.decodeFileThrow ".gitautosync.yaml" :: IO (Maybe ServicePreferences)