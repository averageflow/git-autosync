module Git (areThereUncommittedChanges, commitChanges, pushChanges, addAllChanges) where

import Config
  ( ServiceConfigAddPreferences (argsForAddAction),
    ServiceConfigCommitPreferences (argsForCommitAction, defaultCommitMessage, includeDateInCommitMessage),
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

enrichMessageWithDate :: [Char] -> IO [Char]
enrichMessageWithDate message = do
  dateTime <- getCurrentDateTime
  return $ dateTime ++ " " ++ message

commitChanges :: ServiceConfigCommitPreferences -> IO (ExitCode, String, String)
commitChanges commitPreferences = do
  let shouldAddDateToMessage = includeDateInCommitMessage commitPreferences
  let defaultMessage = defaultCommitMessage commitPreferences
  if shouldAddDateToMessage
    then do
      generatedMessage <- enrichMessageWithDate defaultMessage
      performCommit commitPreferences generatedMessage
    else performCommit commitPreferences defaultMessage

performCommit :: ServiceConfigCommitPreferences -> String -> IO (ExitCode, String, String)
performCommit commitPreferences message = do
  let customArgs = argsForCommitAction commitPreferences
  if null customArgs
    then readProcessWithExitCode "git" ["commit", "-m", message] ""
    else readProcessWithExitCode "git" ("commit" : customArgs ++ ["-m", message]) ""

addAllChanges :: ServiceConfigAddPreferences -> IO (ExitCode, String, String)
addAllChanges addPreferences = do
  let customArgs = argsForAddAction addPreferences
  if null customArgs
    then readProcessWithExitCode "git" ["add", "-A"] ""
    else readProcessWithExitCode "git" ("add" : customArgs) ""

pushChanges :: IO (ExitCode, String, String)
pushChanges = readProcessWithExitCode "git" ["push"] ""
