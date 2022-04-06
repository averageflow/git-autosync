module Lib
  ( gitAutoSynchronizer,
  )
where

import Config
  ( ServiceConfig (servicePreferences),
    ServiceConfigAddPreferences (addAllBeforeCommitting),
    ServiceConfigCommitPreferences (includeDateInCommitMessage),
    ServiceConfigPreferences (addPreferences, commitPreferences, pushPreferences),
    ServiceConfigPushPreferences (pushToRemoteAfterCommit),
    getConfig,
  )
import qualified Data.Maybe
import Git
  ( addAllChanges,
    areThereUncommittedChanges,
    commitChanges,
    pushChanges,
  )
import System.Exit (ExitCode (ExitSuccess), exitFailure)

cheapSeparator :: String
cheapSeparator = "+-------------------------------------------------+"

gitAutoSynchronizer :: IO ()
gitAutoSynchronizer = do
  putStrLn cheapSeparator >> putStrLn "Initiating gitAutoSynchronizer" >> putStrLn ""
  maybeParsedConfig <- getConfig
  case maybeParsedConfig of
    Nothing -> exitFailure
    Just parsedConfig -> do
      shouldProceedToSync <- areThereUncommittedChanges
      if shouldProceedToSync
        then beginSync parsedConfig
        else putStrLn "No uncommitted changes. No action will be taken."
  putStrLn "" >> putStrLn "All actions completed successfully" >> putStrLn cheapSeparator

beginSync :: ServiceConfig -> IO ()
beginSync config = do
  putStrLn "There are uncommitted changes in the repo."
  putStrLn "Preparing to sync changes to upstream."

  if addAllBeforeCommitting . addPreferences . servicePreferences $ config
    then do
      putStrLn "Adding changes..."
      processOutput <- addAllChanges . addPreferences . servicePreferences $ config
      processPrettyPrinter processOutput
    else putStrLn "No additional changes will be added to VCS"

  putStrLn "Committing changes..."
  processOutput <- commitChanges . commitPreferences . servicePreferences $ config
  processPrettyPrinter processOutput

  if pushToRemoteAfterCommit . pushPreferences . servicePreferences $ config
    then do
      putStrLn "Pushing changes..."
      processOutput <- pushChanges . pushPreferences . servicePreferences $ config
      processPrettyPrinter processOutput
    else putStrLn "Will not push to remote due to user's configuration"

processPrettyPrinter :: (ExitCode, String, String) -> IO ()
processPrettyPrinter processOutput = do
  let (exitCode, stdOut, stdErr) = processOutput
  case exitCode of
    ExitSuccess -> do
      putStrLn stdOut
    _ -> do
      putStrLn stdErr