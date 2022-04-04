module Lib
  ( gitAutoSynchronizer,
  )
where

import Config (getConfig)
import Git (addAllChanges, areThereUncommittedChanges, commitChanges, pushChanges)

data AutoSynchronizerActionTrigger = SyncOnUncommittedChanges | SyncOnDiffWithBranch

cheapSeparator :: String
cheapSeparator = "+-------------------------------------------------+"

gitAutoSynchronizer :: IO ()
gitAutoSynchronizer = do
  putStrLn cheapSeparator
  putStrLn "Initiating gitAutoSynchronizer"
  putStrLn ""

  initiateAction SyncOnUncommittedChanges

  putStrLn ""
  putStrLn "All actions completed successfully"
  putStrLn cheapSeparator

initiateAction :: AutoSynchronizerActionTrigger -> IO ()
initiateAction SyncOnUncommittedChanges = do
  shouldProceedToSync <- areThereUncommittedChanges
  getConfig
  -- print shouldProceedToSync
  if not shouldProceedToSync
    then putStrLn "No uncommitted changes. No action will be taken."
    else do
      putStrLn "There are uncommitted changes in the repo."
      putStrLn "Preparing to sync changes to upstream."
      putStrLn "Adding all changes to VCS"
      (exitCode, stdOut, stdErr) <- addAllChanges
      print exitCode
      print stdOut
      print stdErr

      putStrLn "Committing changes"
      (exitCode, stdOut, stdErr) <- commitChanges
      print exitCode
      print stdOut
      print stdErr

      putStrLn "Pushing changes"
      (exitCode, stdOut, stdErr) <- pushChanges
      print exitCode
      print stdOut
      print stdErr

-- Unimplemented conditions
initiateAction _ = putStrLn "Unimplemented feature!"
