module Bench.Prelude
  ( BenchEnv (..),
    runBench,
  )
where

import Shrun qualified as SR
import Shrun.Configuration.Env qualified as Env
import Shrun.Configuration.Env.Types
  ( Env,
    HasAnyError,
    HasCommandLogging,
    HasCommands,
    HasCommonLogging,
    HasConsoleLogging (getConsoleLogging),
    HasFileLogging,
    HasInit,
    HasNotifyConfig,
    HasTimeout,
  )
import Shrun.Logging.RegionLogger
  ( RegionLogger
      ( DisplayRegions,
        LogGlobal,
        LogRegion,
        WithRegion
      ),
  )
import Shrun.Notify.DBus (runDBus)
import Shrun.Notify.Effect (Notify (Notify))
import Shrun.Prelude
import System.Environment qualified as SysEnv

newtype BenchEnv = MkBenchEnv
  {unCoreEnv :: Env ()}
  deriving
    ( HasAnyError,
      HasCommandLogging,
      HasCommands,
      HasCommonLogging,
      HasFileLogging,
      HasInit,
      HasNotifyConfig,
      HasTimeout
    )
    via (Env ())

instance HasConsoleLogging BenchEnv () where
  getConsoleLogging = getConsoleLogging . (.unCoreEnv)

runRegionLogger ::
  ( r ~ (),
    IOE :> es
  ) =>
  Eff (RegionLogger r : es) a ->
  Eff es a
runRegionLogger = interpret $ \env -> \case
  LogGlobal _ -> pure ()
  LogRegion {} -> pure ()
  WithRegion _ regionToShell -> localSeqUnliftIO env $ \unlift ->
    unlift (regionToShell ())
  DisplayRegions m -> localSeqUnliftIO env $ \unlift -> unlift m

runNotify :: Eff (Notify : es) a -> Eff es a
runNotify = interpret_ $ \case
  Notify _ -> pure Nothing

runBench :: List String -> IO ()
runBench argList = do
  SysEnv.withArgs argList $ runShrun $ Env.withEnv $ \env -> do
    let benchEnv = MkBenchEnv env
    runReader benchEnv
      $ runRegionLogger
      $ runNotify
      $ SR.shrun @BenchEnv @()
  where
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
