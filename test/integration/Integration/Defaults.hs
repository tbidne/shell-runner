{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Integration.Defaults (specs) where

import Effects.FileSystem.Utils qualified as FsUtils
import Integration.Prelude
import Integration.Utils
  ( SimpleEnv
      ( MkSimpleEnv,
        cmdLog,
        cmdLogLineTrunc,
        cmdLogSize,
        cmdLogStripControl,
        cmdNameTrunc,
        commands,
        fileLog,
        fileLogStripControl,
        init,
        keyHide,
        notifyAction,
        notifySystem,
        notifyTimeout,
        pollInterval,
        timeout,
        timerFormat
      ),
    makeEnvAndVerify,
    runConfigIO,
    runNoConfigIO,
  )
import Shrun.Data.Command (Command (MkCommand))
import Shrun.Data.KeyHide (KeyHide (KeyHideOff, KeyHideOn))
import Shrun.Data.StripControl
  ( StripControl
      ( StripControlAll,
        StripControlNone,
        StripControlSmart
      ),
  )
import Shrun.Data.TimerFormat
  ( TimerFormat
      ( DigitalCompact,
        DigitalFull,
        ProseCompact
      ),
  )
import Shrun.Notify.Types
  ( NotifyAction (NotifyAll, NotifyCommand, NotifyFinal),
    NotifySystem (AppleScript, DBus, NotifySend),
    NotifyTimeout (NotifyTimeoutNever, NotifyTimeoutSeconds),
  )
import Test.Tasty.Hedgehog (testProperty)

specs :: IO TestArgs -> TestTree
specs testArgs =
  testGroup
    "Default configuration behavior"
    [ defaultEnv,
      usesDefaultConfigFile,
      cliOverridesConfigFile testArgs,
      cliOverridesConfigFileCmdLog,
      cliOverridesConfigFileFileLog,
      ignoresDefaultConfigFile,
      noXOverridesToml,
      noXOverridesArgs
    ]

defaultEnv :: TestTree
defaultEnv = testPropertyNamed desc "defaultEnv"
  $ property
  $ do
    logsRef <- liftIO $ newIORef []
    makeEnvAndVerify ["cmd1"] (`runNoConfigIO` logsRef) expected

    logs <- liftIO $ readIORef logsRef
    ["No default config found at: ./config.toml"] === logs
  where
    desc = "No arguments and empty config path should return default Env"
    expected =
      MkSimpleEnv
        { timeout = Nothing,
          init = Nothing,
          cmdLog = False,
          keyHide = KeyHideOff,
          pollInterval = 10_000,
          cmdLogSize = MkBytes 1024,
          timerFormat = ProseCompact,
          cmdNameTrunc = Nothing,
          cmdLogStripControl = Nothing,
          cmdLogLineTrunc = Nothing,
          fileLog = False,
          fileLogStripControl = Nothing,
          notifySystem = Nothing,
          notifyAction = Nothing,
          notifyTimeout = Nothing,
          commands = "cmd1" :<|| []
        }

{- ORMOLU_DISABLE -}

usesDefaultConfigFile :: TestTree
usesDefaultConfigFile = testPropertyNamed desc "usesDefaultConfigFile"
  $ property
  $ do
    logsRef <- liftIO $ newIORef []
    makeEnvAndVerify ["cmd1"] (`runConfigIO` logsRef) expected

    logs <- liftIO $ readIORef logsRef
    [] === logs
  where
    desc = "No arguments should use config from default file"
    expected =
      MkSimpleEnv
        { timeout = Just 3_600,
          init = Just ". some file",
          keyHide = KeyHideOn,
          pollInterval = 127,
          cmdLogSize = MkBytes 20,
          timerFormat = DigitalFull,
          cmdNameTrunc = Just 80,
          cmdLog = True,
          cmdLogStripControl = Just StripControlAll,
          cmdLogLineTrunc = Just 150,
          fileLog = True,
          fileLogStripControl = Just StripControlNone,
          notifyAction = Just NotifyAll,
#if OSX
          notifySystem = Just AppleScript,
#else
          notifySystem = Just (DBus ()),
#endif
          notifyTimeout = Just NotifyTimeoutNever,
          commands = MkCommand (Just "cmd1") "echo \"command one\"" :<|| []
        }

cliOverridesConfigFile :: IO TestArgs -> TestTree
cliOverridesConfigFile testArgs = testPropertyNamed desc "cliOverridesConfigFile"
  $ property
  $ do
    logPath <- liftIO $ (</> [osp|cli-log|]) . view #workingTmpDir <$> testArgs
    logsRef <- liftIO $ newIORef []
    let logPathStr = FsUtils.unsafeDecodeOsToFp logPath

    makeEnvAndVerify (args logPathStr) (`runConfigIO` logsRef) expected

    logs <- liftIO $ readIORef logsRef
    [] === logs
  where
    desc = "CLI args overrides config file"
    args logPath =
      [ "--config",
        getIntConfigOS "overridden",
        "--timeout",
        "10",
        "--init",
        ". another file",
        "--file-log",
        logPath,
        "--file-log-strip-control",
        "none",
        "--cmd-log",
        "--key-hide",
        "--poll-interval",
        "127",
        "--cmd-log-size",
        "512",
        "--timer-format",
        "digital_compact",
        "--cmd-name-trunc",
        "10",
        "--cmd-log-line-trunc",
        "60",
        "--cmd-log-strip-control",
        "none",
#if !OSX
        "--notify-system",
        "notify-send",
#endif
        "--notify-action",
        "final",
        "--notify-timeout",
        "10",
        "cmd"
      ]
    expected =
      MkSimpleEnv
        { timeout = Just 10,
          init = Just ". another file",
          keyHide = KeyHideOn,
          pollInterval = 127,
          cmdLogSize = MkBytes 512,
          timerFormat = DigitalCompact,
          cmdNameTrunc = Just 10,
          cmdLog = True,
          cmdLogStripControl = Just StripControlNone,
          cmdLogLineTrunc = Just 60,
          fileLog = True,
          fileLogStripControl = Just StripControlNone,
          notifyAction = Just NotifyFinal,
#if OSX
          notifySystem = Just AppleScript,
#else
          notifySystem = Just NotifySend,
#endif
          notifyTimeout = Just (NotifyTimeoutSeconds 10),
          commands = "cmd" :<|| []
        }

cliOverridesConfigFileCmdLog :: TestTree
cliOverridesConfigFileCmdLog = testPropertyNamed desc "cliOverridesConfigFileCmdLog"
  $ property
  $ do
    logsRef <- liftIO $ newIORef []

    makeEnvAndVerify args (`runConfigIO` logsRef) expected

    logs <- liftIO $ readIORef logsRef
    [] === logs
  where
    desc = "CLI overrides config file cmd-log fields even when CLI --cmd-log is not specified"
    args =
      [ "--config",
        getIntConfigOS "overridden",
        "--cmd-log-line-trunc",
        "60",
        "--cmd-log-strip-control",
        "none",
        "cmd"
      ]
    expected =
      MkSimpleEnv
        { -- These two params we care about
          cmdLogStripControl = Just StripControlNone,
          cmdLogLineTrunc = Just 60,
          -- These are just the rest
          timeout = Just 3_600,
          init = Just "blah",
          keyHide = KeyHideOff,
          pollInterval = 100,
          cmdLogSize = MkBytes 50,
          timerFormat = DigitalFull,
          cmdNameTrunc = Just 80,
          cmdLog = True,
          fileLog = True,
          fileLogStripControl = Just StripControlAll,
          notifyAction = Just NotifyCommand,
#if OSX
          notifySystem = Just AppleScript,
#else
          notifySystem = Just (DBus ()),
#endif
          notifyTimeout = Just NotifyTimeoutNever,
          commands = "cmd" :<|| []
        }

cliOverridesConfigFileFileLog :: TestTree
cliOverridesConfigFileFileLog = testPropertyNamed desc "cliOverridesConfigFileFileLog"
  $ property
  $ do
    logsRef <- liftIO $ newIORef []

    makeEnvAndVerify args (`runConfigIO` logsRef) expected

    logs <- liftIO $ readIORef logsRef
    [] === logs
  where
    desc = "CLI overrides config file file-log fields even when CLI --file-log is not specified"
    args =
      [ "--config",
        getIntConfigOS "overridden",
        "--file-log-mode",
        "write",
        "--file-log-strip-control",
        "smart",
        "--file-log-size-mode",
        "warn 10 mb",
        "cmd"
      ]
    expected =
      MkSimpleEnv
        { -- These two params we care about
          fileLog = True,
          fileLogStripControl = Just StripControlSmart,
          -- These are just the rest
          timeout = Just 3_600,
          init = Just "blah",
          keyHide = KeyHideOff,
          pollInterval = 100,
          cmdLogSize = MkBytes 50,
          timerFormat = DigitalFull,
          cmdNameTrunc = Just 80,
          cmdLog = True,
          cmdLogStripControl = Just StripControlAll,
          cmdLogLineTrunc = Just 150,
          notifyAction = Just NotifyCommand,
#if OSX
          notifySystem = Just AppleScript,
#else
          notifySystem = Just (DBus ()),
#endif
          notifyTimeout = Just NotifyTimeoutNever,
          commands = "cmd" :<|| []
        }

{- ORMOLU_ENABLE -}

ignoresDefaultConfigFile :: TestTree
ignoresDefaultConfigFile = testPropertyNamed desc "ignoresDefaultConfigFile"
  $ property
  $ do
    logsRef <- liftIO $ newIORef []
    makeEnvAndVerify ["--no-config", "cmd1"] (`runConfigIO` logsRef) expected

    logs <- liftIO $ readIORef logsRef
    [] === logs
  where
    desc = "--no-config should ignore config file"
    expected =
      MkSimpleEnv
        { timeout = Nothing,
          init = Nothing,
          keyHide = KeyHideOff,
          pollInterval = 10_000,
          cmdLogSize = MkBytes 1024,
          timerFormat = ProseCompact,
          cmdNameTrunc = Nothing,
          cmdLog = False,
          cmdLogStripControl = Nothing,
          cmdLogLineTrunc = Nothing,
          fileLog = False,
          fileLogStripControl = Nothing,
          notifySystem = Nothing,
          notifyAction = Nothing,
          notifyTimeout = Nothing,
          commands = "cmd1" :<|| []
        }

noXOverridesToml :: TestTree
noXOverridesToml = testPropertyNamed desc "noXOverridesToml"
  $ property
  $ do
    logsRef <- liftIO $ newIORef []

    makeEnvAndVerify args (`runConfigIO` logsRef) expected

    logs <- liftIO $ readIORef logsRef
    [] === logs
  where
    desc = "--no-x disables toml options"
    args =
      [ "--config",
        getIntConfigOS "overridden",
        "--no-timeout",
        "--no-init",
        "--no-key-hide",
        "--no-poll-interval",
        "--no-cmd-log-size",
        "--no-timer-format",
        "--no-cmd-name-trunc",
        "--no-cmd-log",
        "--no-cmd-log-strip-control",
        "--no-cmd-log-line-trunc",
        "--no-file-log",
        "--no-file-log-strip-control",
        "--no-notify-action",
        "--no-notify-system",
        "--no-notify-timeout",
        "cmd"
      ]
    expected =
      MkSimpleEnv
        { timeout = Nothing,
          init = Nothing,
          keyHide = KeyHideOff,
          pollInterval = 10_000,
          cmdLogSize = MkBytes 1024,
          timerFormat = ProseCompact,
          cmdNameTrunc = Nothing,
          cmdLog = False,
          cmdLogStripControl = Nothing,
          cmdLogLineTrunc = Nothing,
          fileLog = False,
          fileLogStripControl = Nothing,
          notifyAction = Nothing,
          notifySystem = Nothing,
          notifyTimeout = Nothing,
          commands = "cmd" :<|| []
        }

noXOverridesArgs :: TestTree
noXOverridesArgs = testPropertyNamed desc "noXOverridesArgs"
  $ property
  $ do
    logsRef <- liftIO $ newIORef []

    makeEnvAndVerify args (`runConfigIO` logsRef) expected

    logs <- liftIO $ readIORef logsRef
    [] === logs
  where
    desc = "--no-x disables args"
    args =
      [ "--timeout",
        "5",
        "--no-timeout",
        "--init",
        "blah",
        "--no-init",
        "--poll-interval",
        "555",
        "--no-poll-interval",
        "--cmd-log-size",
        "512",
        "--no-cmd-log-size",
        "--timer-format",
        "prose_full",
        "--no-timer-format",
        "--key-hide",
        "--no-key-hide",
        "--cmd-name-trunc",
        "80",
        "--no-cmd-name-trunc",
        "--cmd-log",
        "--no-cmd-log",
        "--cmd-log-strip-control",
        "all",
        "--no-cmd-log-strip-control",
        "--cmd-log-line-trunc",
        "100",
        "--no-cmd-log-line-trunc",
        "--file-log",
        "path",
        "--no-file-log",
        "--file-log-strip-control",
        "all",
        "--no-file-log-strip-control",
        "--notify-action",
        "command",
        "--no-notify-action",
        "--notify-system",
        "dbus",
        "--no-notify-system",
        "--notify-timeout",
        "never",
        "--no-notify-timeout",
        "cmd"
      ]
    expected =
      MkSimpleEnv
        { timeout = Nothing,
          init = Nothing,
          keyHide = KeyHideOff,
          pollInterval = 10_000,
          cmdLogSize = MkBytes 1024,
          timerFormat = ProseCompact,
          cmdNameTrunc = Nothing,
          cmdLog = False,
          cmdLogStripControl = Nothing,
          cmdLogLineTrunc = Nothing,
          fileLog = False,
          fileLogStripControl = Nothing,
          notifyAction = Nothing,
          notifySystem = Nothing,
          notifyTimeout = Nothing,
          commands = "cmd" :<|| []
        }
