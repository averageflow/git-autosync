module GitAutoSync.CommandRunner where

import ConsolePrinter (errorPrint, fancyPrint, successPrint)
import System.Exit (ExitCode (ExitSuccess), exitFailure)
import System.Process (readProcessWithExitCode)

runSystemCommand :: FilePath -> [String] -> IO ()
runSystemCommand cmd args = do
  (exitCode, stdOut, stdErr) <- readProcessWithExitCode cmd args ""
  case exitCode of
    ExitSuccess -> do
      fancyPrint stdOut
    _ -> do
      errorPrint stdErr
      errorPrint "Aborting operation! Please see the above errors!"
      exitFailure