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
import ConsolePrinter (fancyPrint, fancySeparatorPrint, successPrint)
import qualified Data.Maybe
import Git
  ( addFileContentsToIndex,
    areThereUncommittedChanges,
    commitChanges,
    updateRemoteRefs,
  )
import System.Directory (setCurrentDirectory)
import System.Exit (ExitCode (ExitSuccess), exitFailure, exitSuccess)

gitAutoSynchronizer :: IO ()
gitAutoSynchronizer = do
  fancySeparatorPrint >> fancyPrint "Initiating gitAutoSynchronizer" >> fancyPrint ""
  maybeParsedConfig <- getConfig
  case maybeParsedConfig of
    Nothing -> exitFailure
    Just parsedConfig -> mapM_ beginSync $ managedObjects parsedConfig
  fancyPrint "" >> fancyPrint "All actions completed successfully!" >> fancySeparatorPrint

beginSync :: ManagedObjectPreferences -> IO ()
beginSync objectPreferences = do
  fancyPrint $ "Navigating to: " ++ location objectPreferences
  setCurrentDirectory $ location objectPreferences

  shouldProceedToSync <- areThereUncommittedChanges
  if shouldProceedToSync
    then do
      fancyPrint "There are uncommitted changes in the repo."
      fancyPrint "Preparing to sync changes to upstream."
      if addAllBeforeCommitting . addPreferences $ objectPreferences
        then do
          fancyPrint "Adding changes..."
          addFileContentsToIndex . addPreferences $ objectPreferences
          successPrint "Added changes successfully"
        else fancyPrint "No additional changes will be added to VCS"

      fancyPrint "Committing changes..."
      commitChanges . commitPreferences $ objectPreferences
      successPrint "Committed changes successfully!"

      if pushToRemoteAfterCommit . pushPreferences $ objectPreferences
        then do
          fancyPrint "Pushing changes..."
          updateRemoteRefs . pushPreferences $ objectPreferences
          successPrint "Pushed changes successfully!"
        else fancyPrint "Will not push to remote due to user's configuration"
    else do
      successPrint "No uncommitted changes. No action will be taken."
