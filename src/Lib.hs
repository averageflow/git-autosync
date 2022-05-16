module Lib
  ( gitAutoSynchronizer,
  )
where

import qualified Data.Maybe
import GitAutoSync.Config
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
import GitAutoSync.ConsolePrinter (cyanPrint, fancyPrint, fancyPrint', fancySeparatorPrint, successPrint)
import GitAutoSync.Git
  ( addFileContentsToIndex,
    areThereUncommittedChanges,
    commitChanges,
    updateRemoteRefs,
  )
import System.Directory (setCurrentDirectory)
import System.Exit (ExitCode (ExitSuccess), exitFailure, exitSuccess)

gitAutoSynchronizer :: IO ()
gitAutoSynchronizer = do
  fancySeparatorPrint >> cyanPrint "λ STARTING GIT-AUTOSYNC" >> fancyPrint ""
  maybeParsedConfig <- getConfig
  case maybeParsedConfig of
    Nothing -> exitFailure
    Just parsedConfig -> mapM_ beginSync $ managedObjects parsedConfig
  fancyPrint "" >> cyanPrint "λ TASKS COMPLETED SUCCESSFULLY!" >> fancySeparatorPrint

beginSync objectPreferences = do
  fancyPrint ""
  cyanPrint $ "λ STARTING TASKS FOR OBJECT AT: " ++ location objectPreferences
  setCurrentDirectory $ location objectPreferences

  shouldProceedToSync <- areThereUncommittedChanges
  if shouldProceedToSync
    then do
      fancyPrint' "THERE ARE UNCOMMITTED CHANGES IN THE REPO."
      fancyPrint "PREPARING TO SYNC CHANGES TO UPSTREAM..."
      if addAllBeforeCommitting . addPreferences $ objectPreferences
        then do
          fancyPrint' "ADDING CHANGES..."
          addFileContentsToIndex . addPreferences $ objectPreferences
          successPrint "ADDED CHANGES SUCCESSFULLY!"
        else fancyPrint "NO ADDITIONAL CHANGES WILL BE ADDED TO VCS"

      fancyPrint' "COMMITTING CHANGES..."
      commitChanges . commitPreferences $ objectPreferences
      successPrint "COMMITTED CHANGES SUCCESSFULLY!"

      if pushToRemoteAfterCommit . pushPreferences $ objectPreferences
        then do
          fancyPrint' "PUSHING CHANGES..."
          updateRemoteRefs . pushPreferences $ objectPreferences
          successPrint "PUSHED CHANGES SUCCESSFULLY!"
        else fancyPrint "WILL NOT PUSH TO REMOTE DUE TO USER'S CONFIGURATION"
    else do
      successPrint "NO UNCOMMITTED CHANGES. NO ACTION WILL BE TAKEN"
