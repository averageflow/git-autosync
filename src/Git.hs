module Git (areThereUncommittedChanges, commitChanges, pushChanges, addAllChanges) where

import Config
  ( ServiceConfigCommitPreferences (includeDateInCommitMessage),
  )
import Data.Time (getZonedTime)
import GHC.Base (IO (IO))
import GHC.IO.Exception (ExitCode (ExitSuccess))
import System.Process (readProcessWithExitCode)

areThereUncommittedChanges :: IO Bool
areThereUncommittedChanges = do
  (exitCode, _, _) <- readProcessWithExitCode "git" ["diff", "--quiet"] ""
  case exitCode of
    ExitSuccess -> return False
    _ -> return True

getCurrentDateTime :: IO String
getCurrentDateTime = fmap show getZonedTime

enrichMessageWithDate message = do
  dateTime <- getCurrentDateTime
  return $ dateTime ++ " - " ++ message

commitChanges :: ServiceConfigCommitPreferences -> IO (ExitCode, String, String)
commitChanges commitPreferences = do
  let defaultMessage = "This is a programmatic commit from Haskell code"
  let shouldAddDateToMessage = includeDateInCommitMessage commitPreferences
  if shouldAddDateToMessage
    then do
      generatedMessage <- enrichMessageWithDate defaultMessage
      performCommit generatedMessage
    else performCommit defaultMessage

performCommit message = readProcessWithExitCode "git" ["commit", "-m", message] ""

addAllChanges :: IO (ExitCode, String, String)
addAllChanges = readProcessWithExitCode "git" ["add", "-A"] ""

pushChanges :: IO (ExitCode, String, String)
pushChanges = readProcessWithExitCode "git" ["push"] ""
