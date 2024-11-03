{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Integration.Examples (specs) where

import Data.IORef qualified as IORef
import Integration.Prelude
import Integration.Utils
  ( defaultConfig,
    makeConfigAndAssertEq,
    notifySystemOSDBus,
    notifySystemOSNotifySend,
    runConfigIO,
  )
import Shrun.Configuration.Data.CommandLogging
  ( CommandLoggingP
      ( MkCommandLoggingP,
        bufferLength,
        bufferTimeout,
        pollInterval,
        readSize,
        readStrategy,
        reportReadErrors
      ),
    ReportReadErrorsSwitch (ReportReadErrorsOff),
  )
import Shrun.Configuration.Data.CommandLogging.ReadSize (ReadSize (MkReadSize))
import Shrun.Configuration.Data.CommandLogging.ReadStrategy
  ( ReadStrategy
      ( ReadBlock
      ),
  )
import Shrun.Configuration.Data.CommonLogging
  ( CommonLoggingP
      ( MkCommonLoggingP,
        keyHide
      ),
  )
import Shrun.Configuration.Data.CommonLogging.KeyHideSwitch
  ( KeyHideSwitch (KeyHideOff),
  )
import Shrun.Configuration.Data.ConsoleLogging
  ( ConsoleLogCmdSwitch (ConsoleLogCmdOn),
    ConsoleLoggingP
      ( MkConsoleLoggingP,
        commandLogging,
        commandNameTrunc,
        lineTrunc,
        stripControl,
        timerFormat
      ),
  )
import Shrun.Configuration.Data.ConsoleLogging.TimerFormat
  ( TimerFormat (ProseCompact),
  )
import Shrun.Configuration.Data.Core
  ( CoreConfigP
      ( MkCoreConfigP,
        commandLogging,
        commonLogging,
        consoleLogging,
        fileLogging,
        init,
        notify,
        timeout
      ),
  )
import Shrun.Configuration.Data.MergedConfig
  ( MergedConfig (MkMergedConfig, commands, coreConfig),
  )
import Shrun.Configuration.Data.Notify
  ( NotifyP (MkNotifyP, action, system, timeout),
  )
import Shrun.Configuration.Data.Notify.Action (NotifyAction (NotifyCommand))
import Shrun.Configuration.Data.Notify.System
  ( NotifySystemP (AppleScript, NotifySend),
  )
import Shrun.Configuration.Data.Notify.Timeout
  ( NotifyTimeout (NotifyTimeoutNever),
  )
import Shrun.Configuration.Data.StripControl (StripControl (StripControlSmart))
import Shrun.Data.Command (CommandP (MkCommandP))

specs :: TestTree
specs =
  testGroup
    "Examples"
    [ examplesConfig,
      examplesDefault
    ]

examplesConfig :: TestTree
examplesConfig = testPropertyNamed desc "examplesConfig"
  $ withTests 1
  $ property
  $ do
    logsRef <- liftIO $ IORef.newIORef []
    makeConfigAndAssertEq args (`runConfigIO` logsRef) expected

    logs <- liftIO $ IORef.readIORef logsRef
    [] === logs
  where
    desc = "examples/config.toml is valid"
    args = ["-c", getExampleConfigOS "config", "cmd1"]
    expected =
      MkMergedConfig
        { coreConfig =
            MkCoreConfigP
              { timeout = Just 20,
                init = Just ". examples/bashrc",
                commonLogging =
                  MkCommonLoggingP
                    { keyHide = KeyHideOff
                    },
                consoleLogging =
                  MkConsoleLoggingP
                    { commandLogging = ConsoleLogCmdOn,
                      commandNameTrunc = Just 80,
                      lineTrunc = Just 150,
                      stripControl = StripControlSmart,
                      timerFormat = ProseCompact
                    },
                commandLogging =
                  MkCommandLoggingP
                    { bufferLength = 2000,
                      bufferTimeout = 60,
                      pollInterval = 100,
                      readSize = MkReadSize $ MkBytes 1_000_000,
                      readStrategy = ReadBlock,
                      reportReadErrors = ReportReadErrorsOff
                    },
                fileLogging = Nothing,
                notify =
                  Just
                    $ MkNotifyP
                      { action = NotifyCommand,
                        system = notifySystemOSNotifySend,
                        timeout = NotifyTimeoutNever
                      }
              },
          commands = MkCommandP (Just "cmd1") "echo \"command one\"" :<|| []
        }

examplesDefault :: TestTree
examplesDefault = testPropertyNamed desc "examplesDefault"
  $ withTests 1
  $ property
  $ do
    logsRef <- liftIO $ IORef.newIORef []
    makeConfigAndAssertEq args (`runConfigIO` logsRef) expected

    logs <- liftIO $ IORef.readIORef logsRef
    [] === logs
  where
    desc = "examples/default.toml is valid"
    args = ["-c", getExampleConfig "default", "cmd"]
    expected = defaultConfig
