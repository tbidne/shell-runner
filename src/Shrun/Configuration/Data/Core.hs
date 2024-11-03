{-# LANGUAGE UndecidableInstances #-}

module Shrun.Configuration.Data.Core
  ( -- * Types
    CoreConfigP (..),
    CoreConfigArgs,
    CoreConfigToml,
    CoreConfigMerged,

    -- * Functions
    mergeCoreConfig,
    withCoreEnv,
    defaultCoreConfigMerged,
  )
where

import Shrun.Configuration.Data.CommandLogging (CommandLoggingP, mergeCommandLogging)
import Shrun.Configuration.Data.CommandLogging qualified as CommandLogging
import Shrun.Configuration.Data.CommonLogging (CommonLoggingP, mergeCommonLogging)
import Shrun.Configuration.Data.CommonLogging qualified as CommonLogging
import Shrun.Configuration.Data.ConfigPhase
  ( ConfigPhase
      ( ConfigPhaseArgs,
        ConfigPhaseEnv,
        ConfigPhaseMerged,
        ConfigPhaseToml
      ),
    ConfigPhaseMaybeF,
  )
import Shrun.Configuration.Data.ConsoleLogging (ConsoleLoggingP, mergeConsoleLogging)
import Shrun.Configuration.Data.ConsoleLogging qualified as ConsoleLogging
import Shrun.Configuration.Data.Core.Timeout (Timeout)
import Shrun.Configuration.Data.FileLogging (FileLoggingP, mergeFileLogging)
import Shrun.Configuration.Data.FileLogging qualified as FileLogging
import Shrun.Configuration.Data.Notify (NotifyP, mergeNotifyLogging)
import Shrun.Configuration.Data.Notify qualified as Notify
import Shrun.Configuration.Data.WithDisabled ((<?>?))
import Shrun.Configuration.Default (Default (def))
import Shrun.Data.Command (CommandP1)
import Shrun.Notify.DBus (DBus)
import Shrun.Prelude

-- | For types that are only guaranteed to exist for Args. Generally this
-- describes "aggregate" types e.g. CommandLoggingP, which always exists for
-- Args (as subfields can independently override toml), but is not
-- guaranteed to exist on toml/merged, since its presence in the latter two
-- indicates active status.
type family ArgsOnlyDetF p a where
  ArgsOnlyDetF ConfigPhaseArgs a = a
  ArgsOnlyDetF ConfigPhaseToml a = Maybe a
  ArgsOnlyDetF ConfigPhaseMerged a = Maybe a
  ArgsOnlyDetF ConfigPhaseEnv a = Maybe a

-- | For types that are optional only on the Toml.
type family TomlOptF p a where
  TomlOptF ConfigPhaseArgs a = a
  TomlOptF ConfigPhaseToml a = Maybe a
  TomlOptF ConfigPhaseMerged a = a
  TomlOptF ConfigPhaseEnv a = a

-- | Holds core configuration data.
type CoreConfigP :: ConfigPhase -> Type
data CoreConfigP p = MkCoreConfigP
  { -- | Shell logic to run before each command.
    init :: ConfigPhaseMaybeF p Text,
    -- | Timeout.
    timeout :: ConfigPhaseMaybeF p Timeout,
    -- | Holds common logging config.
    commonLogging :: TomlOptF p (CommonLoggingP p),
    -- | Command log config.
    commandLogging :: TomlOptF p (CommandLoggingP p),
    -- | Holds console logging config.
    consoleLogging :: TomlOptF p (ConsoleLoggingP p),
    -- | File log config.
    fileLogging :: ArgsOnlyDetF p (FileLoggingP p),
    -- | Notify config.
    notify :: ArgsOnlyDetF p (NotifyP p)
  }

instance
  ( k ~ A_Lens,
    a ~ ConfigPhaseMaybeF p Text,
    b ~ ConfigPhaseMaybeF p Text
  ) =>
  LabelOptic "init" k (CoreConfigP p) (CoreConfigP p) a b
  where
  labelOptic =
    lensVL
      $ \f (MkCoreConfigP a1 a2 a3 a4 a5 a6 a7) ->
        fmap
          (\b -> MkCoreConfigP b a2 a3 a4 a5 a6 a7)
          (f a1)
  {-# INLINE labelOptic #-}

instance
  ( k ~ A_Lens,
    a ~ ConfigPhaseMaybeF p Timeout,
    b ~ ConfigPhaseMaybeF p Timeout
  ) =>
  LabelOptic "timeout" k (CoreConfigP p) (CoreConfigP p) a b
  where
  labelOptic =
    lensVL
      $ \f (MkCoreConfigP a1 a2 a3 a4 a5 a6 a7) ->
        fmap
          (\b -> MkCoreConfigP a1 b a3 a4 a5 a6 a7)
          (f a2)
  {-# INLINE labelOptic #-}

instance
  ( k ~ A_Lens,
    a ~ TomlOptF p (CommonLoggingP p),
    b ~ TomlOptF p (CommonLoggingP p)
  ) =>
  LabelOptic "commonLogging" k (CoreConfigP p) (CoreConfigP p) a b
  where
  labelOptic =
    lensVL
      $ \f (MkCoreConfigP a1 a2 a3 a4 a5 a6 a7) ->
        fmap
          (\b -> MkCoreConfigP a1 a2 b a4 a5 a6 a7)
          (f a3)
  {-# INLINE labelOptic #-}

instance
  ( k ~ A_Lens,
    a ~ TomlOptF p (CommandLoggingP p),
    b ~ TomlOptF p (CommandLoggingP p)
  ) =>
  LabelOptic "commandLogging" k (CoreConfigP p) (CoreConfigP p) a b
  where
  labelOptic =
    lensVL
      $ \f (MkCoreConfigP a1 a2 a3 a4 a5 a6 a7) ->
        fmap
          (\b -> MkCoreConfigP a1 a2 a3 b a5 a6 a7)
          (f a4)
  {-# INLINE labelOptic #-}

instance
  ( k ~ A_Lens,
    a ~ TomlOptF p (ConsoleLoggingP p),
    b ~ TomlOptF p (ConsoleLoggingP p)
  ) =>
  LabelOptic "consoleLogging" k (CoreConfigP p) (CoreConfigP p) a b
  where
  labelOptic =
    lensVL
      $ \f (MkCoreConfigP a1 a2 a3 a4 a5 a6 a7) ->
        fmap
          (\b -> MkCoreConfigP a1 a2 a3 a4 b a6 a7)
          (f a5)
  {-# INLINE labelOptic #-}

instance
  ( k ~ A_Lens,
    a ~ ArgsOnlyDetF p (FileLoggingP p),
    b ~ ArgsOnlyDetF p (FileLoggingP p)
  ) =>
  LabelOptic "fileLogging" k (CoreConfigP p) (CoreConfigP p) a b
  where
  labelOptic =
    lensVL
      $ \f (MkCoreConfigP a1 a2 a3 a4 a5 a6 a7) ->
        fmap
          (\b -> MkCoreConfigP a1 a2 a3 a4 a5 b a7)
          (f a6)
  {-# INLINE labelOptic #-}

instance
  ( k ~ A_Lens,
    a ~ ArgsOnlyDetF p (NotifyP p),
    b ~ ArgsOnlyDetF p (NotifyP p)
  ) =>
  LabelOptic "notify" k (CoreConfigP p) (CoreConfigP p) a b
  where
  labelOptic =
    lensVL
      $ \f (MkCoreConfigP a1 a2 a3 a4 a5 a6 a7) ->
        fmap
          (\b -> MkCoreConfigP a1 a2 a3 a4 a5 a6 b)
          (f a7)
  {-# INLINE labelOptic #-}

type CoreConfigArgs = CoreConfigP ConfigPhaseArgs

type CoreConfigToml = CoreConfigP ConfigPhaseToml

type CoreConfigMerged = CoreConfigP ConfigPhaseMerged

type CoreConfigEnv = CoreConfigP ConfigPhaseEnv

deriving stock instance Eq (CoreConfigP ConfigPhaseArgs)

deriving stock instance Show (CoreConfigP ConfigPhaseArgs)

deriving stock instance Eq (CoreConfigP ConfigPhaseToml)

deriving stock instance Show (CoreConfigP ConfigPhaseToml)

deriving stock instance Eq (CoreConfigP ConfigPhaseMerged)

deriving stock instance Show (CoreConfigP ConfigPhaseMerged)

mergeCoreConfig ::
  ( HasCallStack,
    Terminal :> es
  ) =>
  NESeq CommandP1 ->
  CoreConfigArgs ->
  Maybe CoreConfigToml ->
  Eff es CoreConfigMerged
mergeCoreConfig cmds args mToml = do
  consoleLogging <-
    mergeConsoleLogging
      (args ^. #consoleLogging)
      (toml ^. #consoleLogging)

  fileLogging <-
    mergeFileLogging
      (args ^. #fileLogging)
      (toml ^. #fileLogging)

  commandLogging <-
    mergeCommandLogging
      (is _Just fileLogging)
      cmds
      (args ^. #commandLogging)
      (toml ^. #commandLogging)

  pure
    $ MkCoreConfigP
      { timeout = (args ^. #timeout) <?>? (toml ^. #timeout),
        init = (args ^. #init) <?>? (toml ^. #init),
        commonLogging =
          mergeCommonLogging
            (args ^. #commonLogging)
            (toml ^. #commonLogging),
        consoleLogging,
        commandLogging,
        fileLogging,
        notify =
          mergeNotifyLogging
            (args ^. #notify)
            (toml ^. #notify)
      }
  where
    toml = fromMaybe def mToml

-- | Given a merged CoreConfig, constructs a ConfigEnv and calls the
-- continuation.
withCoreEnv ::
  forall es a.
  ( Concurrent :> es,
    DBus :> es,
    FileWriter :> es,
    HandleWriter :> es,
    HasCallStack,
    PathReader :> es,
    PathWriter :> es,
    Terminal :> es
  ) =>
  CoreConfigMerged ->
  (CoreConfigEnv -> Eff es a) ->
  Eff es a
withCoreEnv merged onCoreConfigEnv = do
  notify <- traverse Notify.toEnv (merged ^. #notify)

  FileLogging.withFileLoggingEnv (merged ^. #fileLogging) $ \fileLoggingEnv -> do
    let coreConfigEnv =
          MkCoreConfigP
            { init = merged ^. #init,
              timeout = merged ^. #timeout,
              commonLogging = CommonLogging.toEnv (merged ^. #commonLogging),
              commandLogging = CommandLogging.toEnv (merged ^. #commandLogging),
              consoleLogging = ConsoleLogging.toEnv (merged ^. #consoleLogging),
              fileLogging = fileLoggingEnv,
              notify
            }
     in onCoreConfigEnv coreConfigEnv

instance Default (CoreConfigP ConfigPhaseArgs) where
  def =
    MkCoreConfigP
      { init = def,
        timeout = def,
        commonLogging = def,
        commandLogging = def,
        consoleLogging = def,
        fileLogging = def,
        notify = def
      }

instance Default (CoreConfigP ConfigPhaseToml) where
  def =
    MkCoreConfigP
      { init = def,
        timeout = def,
        commonLogging = def,
        commandLogging = def,
        consoleLogging = def,
        fileLogging = def,
        notify = def
      }

defaultCoreConfigMerged :: NESeq CommandP1 -> CoreConfigP ConfigPhaseMerged
defaultCoreConfigMerged cmds =
  MkCoreConfigP
    { init = def,
      timeout = def,
      commonLogging = def,
      commandLogging =
        CommandLogging.defaultCommandLoggingMerged
          (is _Just fileLogging)
          cmds,
      consoleLogging = def,
      fileLogging,
      notify = def
    }
  where
    fileLogging = def
