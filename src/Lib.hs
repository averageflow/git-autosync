module Lib
  ( gitAutoSynchronizer,
  )
where

import Git (areThereUncommittedChanges, commitChanges, pushChanges)

data AutoSynchronizerActionTrigger = SyncOnUncommittedChanges | SyncOnDiffWithBranch

cheapSeparator = "+-------------------------------------------------+"

gitAutoSynchronizer = do
  putStrLn cheapSeparator
  putStrLn "Initiating gitAutoSynchronizer"
  putStrLn ""

  initiateAction SyncOnUncommittedChanges

  putStrLn ""
  putStrLn "All actions completed successfully"
  putStrLn cheapSeparator

initiateAction SyncOnUncommittedChanges = do
  shouldProceedToSync <- areThereUncommittedChanges
  -- print shouldProceedToSync
  if not shouldProceedToSync
    then putStrLn "No uncommitted changes. No action will be taken."
    else do
      putStrLn "There are uncommitted changes in the repo."
      putStrLn "Preparing to sync changes to upstream."
      (exitCode, stdOut, stdErr) <- commitChanges
      print exitCode
      print stdOut
      print stdErr

      (exitCode, stdOut, stdErr) <- pushChanges
      print exitCode
      print stdOut
      print stdErr
-- Unimplemented conditions
initiateAction _ = putStrLn "Unimplemented feature!"
