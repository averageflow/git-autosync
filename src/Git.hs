module Git (areThereUncommittedChanges, commitChanges, pushChanges, addAllChanges) where

import Data.Time (getZonedTime)
import GHC.Base (IO (IO))
import GHC.IO.Exception (ExitCode (ExitFailure, ExitSuccess))
import System.Process (readProcessWithExitCode)
import Text.Printf

areThereUncommittedChanges :: IO Bool
areThereUncommittedChanges = do
  (exitCode, _, _) <- readProcessWithExitCode "git" ["diff", "--quiet"] ""
  if exitCode == ExitSuccess
    then return False
    else return True

getCurrentDateTime :: IO String
getCurrentDateTime = fmap show getZonedTime

commitMessage :: IO [Char]
commitMessage = do
  dateTime <- getCurrentDateTime
  return ("This is a programmatic commit, made with Haskell code, on " ++ dateTime)

commitChanges :: IO (ExitCode, String, String)
commitChanges = do
  generatedCommitMessage <- commitMessage
  readProcessWithExitCode "git" ["commit", "-am", generatedCommitMessage] ""

addAllChanges :: IO (ExitCode, String, String)
addAllChanges = readProcessWithExitCode "git" ["add", "-A"] ""

pushChanges :: IO (ExitCode, String, String)
pushChanges = readProcessWithExitCode "git" ["push"] ""
