module Lib
  ( gitAutoSynchronizer,
  )
where

import Config
  ( ServiceConfig (servicePreferences),
    ServiceConfigAddPreferences (addAllBeforeCommitting),
    ServiceConfigCommitPreferences (includeDateInCommitMessage),
    ServiceConfigPreferences (addPreferences, commitPreferences, pushPreferences),
    ServiceConfigPushPreferences (pushToRemoteAfterCommit),
    getConfig,
  )
import qualified Data.Maybe
import Git
  ( addAllChanges,
    areThereUncommittedChanges,
    commitChanges,
    pushChanges,
  )
import System.Exit (ExitCode, exitFailure)

cheapSeparator :: String
cheapSeparator = "+-------------------------------------------------+"

gitAutoSynchronizer :: IO ()
gitAutoSynchronizer = do
  putStrLn cheapSeparator >> putStrLn "Initiating gitAutoSynchronizer" >> putStrLn ""
  maybeParsedConfig <- getConfig
  case maybeParsedConfig of
    Nothing -> exitFailure
    Just parsedConfig -> do
      shouldProceedToSync <- areThereUncommittedChanges
      if shouldProceedToSync
        then beginSync parsedConfig
        else putStrLn "No uncommitted changes. No action will be taken."
  putStrLn "" >> putStrLn "All actions completed successfully" >> putStrLn cheapSeparator

beginSync :: ServiceConfig -> IO ()
beginSync config = do
  putStrLn "There are uncommitted changes in the repo."
  putStrLn "Preparing to sync changes to upstream."

  if addAllBeforeCommitting . addPreferences . servicePreferences $ config
    then do
      putStrLn "Adding changes..."
      (exitCode, stdOut, stdErr) <- addAllChanges . addPreferences . servicePreferences $ config
      print exitCode >> print stdOut >> print stdErr
    else putStrLn "No additional changes will be added to VCS"

  putStrLn "Committing changes..."
  (exitCode, stdOut, stdErr) <- commitChanges . commitPreferences . servicePreferences $ config
  print exitCode >> print stdOut >> print stdErr

  if pushToRemoteAfterCommit . pushPreferences . servicePreferences $ config
    then do
      putStrLn "Pushing changes..."
      processOutput <- pushChanges . pushPreferences . servicePreferences $ config
      -- print exitCode >> print stdOut >> print stdErr
      cliProcessPrettyPrinter processOutput
    else putStrLn "Will not push to remote due to user's configuration"

cliProcessPrettyPrinter :: (ExitCode, String, String) -> IO ()
cliProcessPrettyPrinter processOutput = do
  -- print exitCode >> print stdOut >> print stdErr
  let (exitCode, stdOut, stdErr) = processOutput
  print stdOut