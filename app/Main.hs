module Main (main) where

import Shrun.Configuration.Env (makeEnvAndShrun)
import Shrun.Logging.RegionLogger (runRegionLogger)
import Shrun.Notify.DBus (runDBus)
import Shrun.Prelude hiding (IO)
import Prelude (IO)

main :: IO ()
main = do
  -- NOTE: Seems redundant, but sadly it isn't.
  --
  -- doNothingOnSuccess is solely used to stop ExitSuccess from poisoning the
  -- error code because GHC determines exit success/failure based on the
  -- presence of an uncaught exception. However, we still need to rethrow
  -- the failure.
  --
  -- Moreover, we don't really want to print a CallStack for ExitFailure,
  -- as we are throwing that whenever a command fails, and the CallStack
  -- is just unhelpful noise.
  setUncaughtExceptionHandlerDisplay

  runShrun (makeEnvAndShrun @ConsoleRegion) `catch` doNothingOnSuccess
  where
    -- We need to catch ExitCode so that optparse applicative's --help
    -- does not set the error code to failure...but then we need to rethrow
    -- failures.
    doNothingOnSuccess :: ExitCode -> IO ()
    doNothingOnSuccess ExitSuccess = pure ()
    doNothingOnSuccess ex@(ExitFailure _) = throwM ex

    runShrun =
      runEff
        . runConcurrent
        . runTypedProcess
        . runIORef
        . runOptparse
        . runTime
        . runFileReader
        . runFileWriter
        . runHandleReader
        . runHandleWriter
        . runPathReader
        . runPathWriter
        . runTerminal
        . runRegionLogger
        . runDBus
