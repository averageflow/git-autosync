module Git (areThereUncommittedChanges, commitChanges, updateRemoteRefs, addFileContentsToIndex) where

import CommandRunner (runSystemCommand)
import Config
  ( AddPreferences (argsForAddAction),
    CommitPreferences
      ( argsForCommitAction,
        defaultCommitMessage,
        includeDateInCommitMessage
      ),
    PushPreferences (argsForPushAction),
  )
import Data.Time (getZonedTime)
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

commitChanges :: CommitPreferences -> IO ()
commitChanges commitPreferences = do
  let shouldAddDateToMessage = includeDateInCommitMessage commitPreferences
  let defaultMessage = defaultCommitMessage commitPreferences
  if shouldAddDateToMessage
    then do
      generatedMessage <- enrichMessageWithDate defaultMessage
      recordChangesToRepository commitPreferences generatedMessage
    else recordChangesToRepository commitPreferences defaultMessage

-- Record changes to the repository
recordChangesToRepository :: CommitPreferences -> String -> IO ()
recordChangesToRepository commitPreferences message = do
  let customArgs = argsForCommitAction commitPreferences
  if null customArgs
    then runSystemCommand "git" ["commit", "-m", message]
    else runSystemCommand "git" ("commit" : customArgs ++ ["-m", message])

-- Add file contents to the index
addFileContentsToIndex :: AddPreferences -> IO ()
addFileContentsToIndex addPreferences = do
  let customArgs = argsForAddAction addPreferences
  if null customArgs
    then -- add all by default
      runSystemCommand "git" ["add", "-A"]
    else -- use custom args from .yaml file
      runSystemCommand "git" ("add" : customArgs)

-- Update remote refs along with associated objects
updateRemoteRefs :: PushPreferences -> IO ()
updateRemoteRefs pushPreferences = runSystemCommand "git" ("push" : argsForPushAction pushPreferences)