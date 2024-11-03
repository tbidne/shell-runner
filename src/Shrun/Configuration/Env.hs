{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides functions for creating 'Env' from CLI/Toml configuration.
module Shrun.Configuration.Env
  ( -- * Running with Env
    withEnv,
    makeEnvAndShrun,

    -- * Misc
    getMergedConfig,
  )
where

import Data.Sequence qualified as Seq
import Data.Text qualified as T
import Shrun (shrun)
import Shrun.Configuration (mergeConfig)
import Shrun.Configuration.Args.Parsing
  ( parserInfoArgs,
  )
import Shrun.Configuration.Data.Core qualified as CoreConfig
import Shrun.Configuration.Data.MergedConfig (MergedConfig)
import Shrun.Configuration.Data.WithDisabled
  ( WithDisabled (Disabled, With, Without),
  )
import Shrun.Configuration.Env.Types
  ( Env
      ( MkEnv,
        anyError,
        commands,
        completedCommands,
        config,
        consoleLogQueue
      ),
  )
import Shrun.Logging.RegionLogger (RegionLogger)
import Shrun.Notify (runNotify)
import Shrun.Notify.DBus (DBus)
import Shrun.Prelude

-- | 'withEnv' with 'shrun'.
makeEnvAndShrun ::
  forall r es.
  ( Concurrent :> es,
    DBus :> es,
    FileReader :> es,
    FileWriter :> es,
    HandleReader :> es,
    HandleWriter :> es,
    HasCallStack,
    IORefE :> es,
    Optparse :> es,
    PathReader :> es,
    PathWriter :> es,
    TypedProcess :> es,
    RegionLogger r :> es,
    Terminal :> es,
    Time :> es
  ) =>
  Eff es ()
makeEnvAndShrun = withEnv @r $ \env ->
  runReader env (runNotify @r $ shrun @(Env r) @r)

-- | Creates an 'Env' from CLI args and TOML config to run with a monadic
-- action.
withEnv ::
  forall r es a.
  ( Concurrent :> es,
    DBus :> es,
    FileReader :> es,
    FileWriter :> es,
    HandleWriter :> es,
    HasCallStack,
    Optparse :> es,
    PathReader :> es,
    PathWriter :> es,
    Terminal :> es
  ) =>
  (Env r -> Eff es a) ->
  Eff es a
withEnv onEnv = getMergedConfig >>= flip fromMergedConfig onEnv

-- | Creates a 'MergedConfig' from CLI args and TOML config.
getMergedConfig ::
  ( FileReader :> es,
    HasCallStack,
    Optparse :> es,
    PathReader :> es,
    Terminal :> es
  ) =>
  Eff es MergedConfig
getMergedConfig = do
  args <- execParser parserInfoArgs

  mTomlConfig <-
    case args ^. #configPath of
      -- 1. If noConfig is true then we ignore all toml config
      Disabled -> pure Nothing
      -- 2. noConfig is false and toml config not set: try reading from
      --    default location. If it does not exist that's fine, just print
      --    a message. If it does, try to read it and throw any errors
      --    (e.g. file errors, toml errors).
      Without -> do
        configDir <- getShrunXdgConfig
        let path = configDir </> [osp|config.toml|]
        b <- doesFileExist path
        if b
          then Just <$> readConfig path
          else do
            putTextLn
              ( "No default config found at: '"
                  <> T.pack (decodeLenient path)
                  <> "'"
              )
            pure Nothing
      -- 3. noConfig is false and toml config explicitly set: try reading
      --    (all errors rethrown)
      With f -> readConfig f

  mergeConfig args mTomlConfig
  where
    readConfig fp = do
      contents <- readFileUtf8ThrowM fp
      case decode contents of
        Right cfg -> pure cfg
        Left tomlErr -> throwM tomlErr

fromMergedConfig ::
  ( Concurrent :> es,
    DBus :> es,
    FileWriter :> es,
    HandleWriter :> es,
    HasCallStack,
    PathReader :> es,
    PathWriter :> es,
    Terminal :> es
  ) =>
  MergedConfig ->
  (Env r -> Eff es a) ->
  Eff es a
fromMergedConfig cfg onEnv = do
  completedCommands <- newTVarA Seq.empty
  anyError <- newTVarA False
  consoleLogQueue <- newTBQueueA 1_000

  CoreConfig.withCoreEnv (cfg ^. #coreConfig) $ \coreConfigEnv -> do
    let env =
          MkEnv
            { config = coreConfigEnv,
              anyError,
              completedCommands,
              consoleLogQueue,
              commands
            }

    onEnv env
  where
    commands = cfg ^. #commands

getShrunXdgConfig :: (HasCallStack, PathReader :> es) => Eff es OsPath
getShrunXdgConfig = getXdgConfig [osp|shrun|]
