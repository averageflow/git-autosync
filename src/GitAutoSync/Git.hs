module Git (areThereUncommittedChanges, commitChanges, updateRemoteRefs, addFileContentsToIndex) where

import CommandRunner (runSystemCommand)
import Config
  ( AddPreferences (argsForAddAction),
    CommitPreferences
      ( CommitPreferences,
        argsForCommitAction,
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
commitChanges (CommitPreferences includeDateInCommitMessage defaultCommitMessage commitArgs) = do
  msg <- makeMessage
  recordChangesToRepository commitArgs msg
  where
    makeMessage
      | includeDateInCommitMessage = enrichMessageWithDate defaultCommitMessage
      | otherwise = pure defaultCommitMessage

-- Record changes to the repository
recordChangesToRepository :: [String] -> String -> IO ()
recordChangesToRepository customArgs message = do
  if null customArgs
    then runSystemCommand "git" ["commit", "-m", message]
    else runSystemCommand "git" ("commit" : customArgs ++ ["-m", message])

-- Add file contents to the index
addFileContentsToIndex :: AddPreferences -> IO ()
addFileContentsToIndex addPreferences = do
  let customArgs = argsForAddAction addPreferences
  let args = if null customArgs then ["-A"] else customArgs
  runSystemCommand "git" ("add" : args)

-- Update remote refs along with associated objects
updateRemoteRefs :: PushPreferences -> IO ()
updateRemoteRefs pushPreferences = runSystemCommand "git" ("push" : argsForPushAction pushPreferences)