module Lib
  ( gitAutoSynchronizer,
  )
where

import Config
  ( ServiceConfig (servicePreferences),
    ServiceConfigAddPreferences (addAllBeforeCommitting),
    ServiceConfigCommitPreferences (includeDateInCommitMessage),
    ServiceConfigPreferences (addPreferences, commitPreferences),
    getConfig,
  )
import qualified Data.Maybe
import Git
  ( addAllChanges,
    areThereUncommittedChanges,
    commitChanges,
    pushChanges,
  )
import System.Exit (exitFailure)

data AutoSynchronizerActionTrigger = SyncOnUncommittedChanges | SyncOnDiffWithBranch

cheapSeparator :: String
cheapSeparator = "+-------------------------------------------------+"

gitAutoSynchronizer :: IO ()
gitAutoSynchronizer = do
  putStrLn cheapSeparator >> putStrLn "Initiating gitAutoSynchronizer" >> putStrLn ""
  initiateAction SyncOnUncommittedChanges
  putStrLn "" >> putStrLn "All actions completed successfully" >> putStrLn cheapSeparator

initiateAction :: AutoSynchronizerActionTrigger -> IO ()
initiateAction SyncOnUncommittedChanges = do
  maybeParsedConfig <- getConfig
  maybe exitFailure print maybeParsedConfig

  let (Just parsedConfig) = maybeParsedConfig

  shouldProceedToSync <- areThereUncommittedChanges

  -- print shouldProceedToSync
  if not shouldProceedToSync
    then putStrLn "No uncommitted changes. No action will be taken."
    else beginSync parsedConfig

-- Unimplemented conditions
initiateAction _ = putStrLn "Unimplemented feature!"

beginSync :: ServiceConfig -> IO ()
beginSync config = do
  putStrLn "There are uncommitted changes in the repo."
  putStrLn "Preparing to sync changes to upstream."

  if addAllBeforeCommitting . addPreferences . servicePreferences $ config
    then do
      putStrLn "Adding all changes to VCS"
      (exitCode, stdOut, stdErr) <- addAllChanges . addPreferences . servicePreferences $ config
      print exitCode >> print stdOut >> print stdErr
    else putStrLn "No additional changes will be added to VCS"

  putStrLn "Committing changes"
  (exitCode, stdOut, stdErr) <- commitChanges . commitPreferences . servicePreferences $ config
  print exitCode >> print stdOut >> print stdErr

  putStrLn "Pushing changes"
  (exitCode, stdOut, stdErr) <- pushChanges
  print exitCode >> print stdOut >> print stdErr