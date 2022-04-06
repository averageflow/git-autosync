module Lib
  ( gitAutoSynchronizer,
  )
where

import Config
  ( AddPreferences (addAllBeforeCommitting),
    ManagedObjectPreferences
      ( addPreferences,
        commitPreferences,
        location,
        pushPreferences
      ),
    PushPreferences (pushToRemoteAfterCommit),
    ServicePreferences (managedObjects),
    getConfig,
  )
import qualified Data.Maybe
import Git
  ( addAllChanges,
    areThereUncommittedChanges,
    commitChanges,
    pushChanges,
  )
import System.Directory (setCurrentDirectory)
import System.Exit (ExitCode (ExitSuccess), exitFailure, exitSuccess)

cheapSeparator :: String
cheapSeparator = "+-------------------------------------------------+"

gitAutoSynchronizer = do
  putStrLn cheapSeparator >> putStrLn "Initiating gitAutoSynchronizer" >> putStrLn ""
  maybeParsedConfig <- getConfig
  case maybeParsedConfig of
    Nothing -> exitFailure
    Just parsedConfig -> mapM_ beginSync $ managedObjects parsedConfig
  putStrLn "" >> putStrLn "All actions completed successfully!" >> putStrLn cheapSeparator

beginSync objectPreferences = do
  putStrLn $ "Navigating to: " ++ location objectPreferences
  setCurrentDirectory $ location objectPreferences

  shouldProceedToSync <- areThereUncommittedChanges
  if shouldProceedToSync
    then do
      putStrLn "There are uncommitted changes in the repo."
      putStrLn "Preparing to sync changes to upstream."
      if addAllBeforeCommitting . addPreferences $ objectPreferences
        then do
          putStrLn "Adding changes..."
          processOutput <- addAllChanges . addPreferences $ objectPreferences
          processOutputHandler processOutput
        else putStrLn "No additional changes will be added to VCS"

      putStrLn "Committing changes..."
      processOutput <- commitChanges . commitPreferences $ objectPreferences
      processOutputHandler processOutput

      if pushToRemoteAfterCommit . pushPreferences $ objectPreferences
        then do
          putStrLn "Pushing changes..."
          processOutput <- pushChanges . pushPreferences $ objectPreferences
          processOutputHandler processOutput
        else putStrLn "Will not push to remote due to user's configuration"
    else do
      putStrLn "No uncommitted changes. No action will be taken."

processOutputHandler :: (ExitCode, String, String) -> IO ()
processOutputHandler processOutput = do
  let (exitCode, stdOut, stdErr) = processOutput
  case exitCode of
    ExitSuccess -> do
      putStrLn stdOut
    _ -> do
      putStrLn stdErr
      putStrLn "Aborting operation! Please see the above errors!"
      exitFailure