{-# LANGUAGE CPP #-}
-- see NOTE: [Unused Top Binds]
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- | Runs functional tests.
module Main (main) where

import Data.IORef qualified as IORef
import Data.Text qualified as T
import GHC.Conc.Sync (setUncaughtExceptionHandler)
import Shrun qualified as SR
import Shrun.Configuration.Data.Notify.System
  ( NotifySystemP
      ( AppleScript,
        DBus,
        NotifySend
      ),
  )
import Shrun.Configuration.Env (withEnv)
import Shrun.Configuration.Env.Types
  ( Env,
    HasAnyError (getAnyError),
    HasCommandLogging (getCommandLogging),
    HasCommands (getCommands, getCompletedCommands),
    HasCommonLogging (getCommonLogging),
    HasConsoleLogging (getConsoleLogging),
    HasFileLogging (getFileLogging),
    HasInit (getInit),
    HasNotifyConfig (getNotifyConfig),
    HasTimeout (getTimeout),
  )
import Shrun.Logging.RegionLogger
  ( RegionLogger
      ( DisplayRegions,
        LogGlobal,
        LogRegion,
        WithRegion
      ),
  )
import Shrun.Logging.Types (LogRegion)
import Shrun.Notify.AppleScript qualified as AppleScript
import Shrun.Notify.DBus qualified as DBus
import Shrun.Notify.Effect (Notify (Notify))
import Shrun.Notify.NotifySend qualified as NotifySend
import Shrun.Prelude
import System.Environment qualified as SysEnv
import System.Environment.Guard (guardOrElse')
import System.Environment.Guard.Lifted (ExpectEnv (ExpectEnvSet))
import System.IO qualified as IO
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)

-- | Entry point for functional tests.
main :: IO ()
main = guardOrElse' "NOTIFY_TESTS" ExpectEnvSet runTests dontRun
  where
    runTests = do
      setUncaughtExceptionHandler (IO.putStrLn . displayException)
      defaultMain tests

    dontRun = IO.putStrLn "*** Notify tests disabled. Enable with NOTIFY_TESTS=1 ***"

tests :: TestTree
tests = do
  testGroup
    "Notify Tests"
    osTests

osTests :: List TestTree

#if OSX
osTests =
  [ mkTest "apple-script"
  ]
#else
osTests =
  [ mkTest "dbus",
    mkTest "notify-send",
    notifySendHandlesLegendQuotes
  ]

-- This test for a bug where notify-send could not cope with quotation marks
-- in legend file commands.
notifySendHandlesLegendQuotes :: TestTree
notifySendHandlesLegendQuotes = testCase "notify-send handles legend quotes" $ do
  runNotifyIO args
  where
    args =
      [ "--common-log-key-hide",
        "--notify-action",
        "all",
        "--notify-system",
        "notify-send",
        "--config",
        "examples/config.toml",
        "ui"
      ]
#endif

mkTest :: String -> TestTree
mkTest system = testCase ("Runs notify with " ++ system) $ do
  runShrunNoConfig (mkArgs system)

mkArgs :: String -> List String
mkArgs system =
  [ "--notify-action",
    "all",
    "--notify-system",
    system,
    "--notify-timeout",
    "5",
    "sleep 2",
    "sleep 3"
  ]

-- NOTE: [Unused Top Binds]
--
-- Apparently, this warning is tripped as GHC accurately determines that
-- the consoleQueue field name is never used. It's kind of silly though since
-- we are using the other fields, and it's not like we can only create
-- one or two. Consider filing a GHC issue for this.

data NotifyEnv = MkNotifyEnv
  { unNotifyEnv :: Env (),
    consoleQueue :: TBQueue (LogRegion ()),
    logsRef :: IORef (List Text)
  }

instance HasAnyError NotifyEnv where
  getAnyError = getAnyError . (.unNotifyEnv)

instance HasCommands NotifyEnv where
  getCommands = getCommands . (.unNotifyEnv)
  getCompletedCommands = getCompletedCommands . (.unNotifyEnv)

instance HasCommandLogging NotifyEnv where
  getCommandLogging = getCommandLogging . (.unNotifyEnv)

instance HasCommonLogging NotifyEnv where
  getCommonLogging = getCommonLogging . (.unNotifyEnv)

instance HasConsoleLogging NotifyEnv () where
  getConsoleLogging = getConsoleLogging . (.unNotifyEnv)

instance HasFileLogging NotifyEnv where
  getFileLogging = getFileLogging . (.unNotifyEnv)

instance HasInit NotifyEnv where
  getInit = getInit . (.unNotifyEnv)

instance HasNotifyConfig NotifyEnv where
  getNotifyConfig = getNotifyConfig . (.unNotifyEnv)

instance HasTimeout NotifyEnv where
  getTimeout = getTimeout . (.unNotifyEnv)

runNotify ::
  ( DBus.DBus :> es,
    Reader NotifyEnv :> es,
    TypedProcess :> es
  ) =>
  Eff (Notify : es) a ->
  Eff es a
runNotify = interpret_ $ \case
  Notify note -> do
    asks @NotifyEnv (preview (#config % #notify %? #system) . (.unNotifyEnv)) >>= \case
      Nothing -> pure Nothing
      Just nenv -> sendNote nenv
    where
      sendNote (DBus client) = DBus.notifyDBus client note
      sendNote NotifySend = NotifySend.notifyNotifySend note
      sendNote AppleScript = AppleScript.notifyAppleScript note

runRegionLogger ::
  ( r ~ (),
    HasCallStack,
    IOE :> es,
    IORefE :> es,
    Reader NotifyEnv :> es
  ) =>
  Eff (RegionLogger r : es) a ->
  Eff es a
runRegionLogger = interpret $ \env -> \case
  LogGlobal txt -> writeLogs txt
  LogRegion _ _ txt -> writeLogs txt
  WithRegion _ regionToShell -> localSeqUnliftIO env $ \unlift ->
    unlift (regionToShell ())
  DisplayRegions m -> localSeqUnliftIO env $ \unlift -> unlift m
  where
    writeLogs txt = do
      ls <- asks @NotifyEnv $ (.logsRef)
      modifyIORef' ls (txt :)

runShrunNoConfig :: List String -> IO ()
runShrunNoConfig = runNotifyIO . ("--no-config" :)

runNotifyIO :: List String -> IO ()
runNotifyIO args = do
  logsRef <- IORef.newIORef []
  eSomeEx <- trySync $ SysEnv.withArgs args $ runShrun $ withEnv $ \env -> do
    consoleQueue <- newTBQueueA 1
    let notifyEnv = MkNotifyEnv env consoleQueue logsRef
    runReader notifyEnv
      $ runRegionLogger
      $ runNotify
      $ SR.shrun @NotifyEnv @()

  case eSomeEx of
    Right () -> pure ()
    Left ex -> do
      logs <- IORef.readIORef logsRef

      let formatted = T.intercalate "\n" logs
          err =
            mconcat
              [ "Encountered exception\n\n",
                "Logs:\n\n",
                T.unpack formatted,
                "\n\nException message: ",
                displayException ex
              ]

      assertFailure err
  where
    runShrun =
      runEff
        . runConcurrent
        . runTypedProcess
        . runIORef
        . runFileReader
        . runFileWriter
        . runHandleReader
        . runHandleWriter
        . runOptparse
        . runPathReader
        . runPathWriter
        . runTerminal
        . runTime
        . DBus.runDBus
