module Lib
  ( gitAutoSynchronizer,
  )
where

import qualified Data.Functor
import Data.Time (getZonedTime)
import Data.Time.Calendar
import Data.Time.Clock
import GHC.Base (IO (IO))
import GHC.IO.Exception (ExitCode (ExitFailure, ExitSuccess))
import System.Process (readProcessWithExitCode)
import Text.Printf

data AutoSynchronizerActionTrigger = SyncOnUncommittedChanges | SyncOnDiffWithBranch

cheapSeparator = "+-------------------------------------------------+"

getCurrentDateTime = fmap show getZonedTime

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

areThereUncommittedChanges :: IO Bool
areThereUncommittedChanges = do
  (shouldProceedToSync, _, _) <- readProcessWithExitCode "git" ["diff", "--quiet"] ""
  if shouldProceedToSync == ExitSuccess
    then return False
    else return True

commitMessage = do
  dateTime <- getCurrentDateTime
  return ("This is a programmatic commit, made with Haskell code, on " ++ dateTime)

commitChanges = do
  generatedCommitMessage <- commitMessage
  readProcessWithExitCode "git" ["commit", "-am", generatedCommitMessage] ""

pushChanges = readProcessWithExitCode "git" ["push"] ""
