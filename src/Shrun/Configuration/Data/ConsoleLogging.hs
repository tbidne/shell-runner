{-# LANGUAGE UndecidableInstances #-}

module Shrun.Configuration.Data.ConsoleLogging
  ( -- * Types
    ConsoleLoggingP (..),
    ConsoleLoggingArgs,
    ConsoleLoggingToml,
    ConsoleLoggingMerged,
    ConsoleLoggingEnv,
    ConsoleLogCmdSwitch (..),

    -- * Functions
    mergeConsoleLogging,
    toEnv,
  )
where

import Shrun.Configuration.Data.ConfigPhase
  ( ConfigPhase
      ( ConfigPhaseArgs,
        ConfigPhaseEnv,
        ConfigPhaseMerged,
        ConfigPhaseToml
      ),
    ConfigPhaseF,
    ConfigPhaseMaybeF,
    LineTruncF,
    SwitchF,
  )
import Shrun.Configuration.Data.ConsoleLogging.TimerFormat (TimerFormat)
import Shrun.Configuration.Data.StripControl (ConsoleLogStripControl)
import Shrun.Configuration.Data.Truncation
  ( TruncRegion (TruncCommandName),
    Truncation,
    configToLineTrunc,
    decodeCommandNameTrunc,
    decodeLineTrunc,
  )
import Shrun.Configuration.Data.WithDisabled (WithDisabled, (<.>?), (<>?), (<?>?))
import Shrun.Configuration.Data.WithDisabled qualified as WD
import Shrun.Configuration.Default (Default (def))
import Shrun.Prelude

-- | Switch for command logging in console logs.
data ConsoleLogCmdSwitch
  = ConsoleLogCmdOff
  | ConsoleLogCmdOn
  deriving stock (Eq, Show)

instance Default ConsoleLogCmdSwitch where
  def = ConsoleLogCmdOff

instance
  ( k ~ An_Iso,
    a ~ Bool,
    b ~ Bool
  ) =>
  LabelOptic
    "boolIso"
    k
    ConsoleLogCmdSwitch
    ConsoleLogCmdSwitch
    a
    b
  where
  labelOptic =
    iso
      (\cases ConsoleLogCmdOn -> True; ConsoleLogCmdOff -> False)
      (\cases True -> ConsoleLogCmdOn; False -> ConsoleLogCmdOff)

-- | Holds command logging config.
type ConsoleLoggingP :: ConfigPhase -> Type
data ConsoleLoggingP p = MkConsoleLoggingP
  { -- | Whether command logging is enabled.
    commandLogging :: SwitchF p ConsoleLogCmdSwitch,
    -- | Command name truncation.
    commandNameTrunc :: ConfigPhaseMaybeF p (Truncation TruncCommandName),
    -- | Line truncation.
    lineTrunc :: LineTruncF p,
    -- | Strip control.
    stripControl :: ConfigPhaseF p ConsoleLogStripControl,
    -- | How to format the timer.
    timerFormat :: ConfigPhaseF p TimerFormat
  }

instance
  ( k ~ A_Lens,
    a ~ SwitchF p ConsoleLogCmdSwitch,
    b ~ SwitchF p ConsoleLogCmdSwitch
  ) =>
  LabelOptic "commandLogging" k (ConsoleLoggingP p) (ConsoleLoggingP p) a b
  where
  labelOptic =
    lensVL
      $ \f (MkConsoleLoggingP a1 a2 a3 a4 a5) ->
        fmap
          (\b -> MkConsoleLoggingP b a2 a3 a4 a5)
          (f a1)
  {-# INLINE labelOptic #-}

instance
  ( k ~ A_Lens,
    a ~ ConfigPhaseMaybeF p (Truncation TruncCommandName),
    b ~ ConfigPhaseMaybeF p (Truncation TruncCommandName)
  ) =>
  LabelOptic "commandNameTrunc" k (ConsoleLoggingP p) (ConsoleLoggingP p) a b
  where
  labelOptic =
    lensVL
      $ \f (MkConsoleLoggingP a1 a2 a3 a4 a5) ->
        fmap
          (\b -> MkConsoleLoggingP a1 b a3 a4 a5)
          (f a2)
  {-# INLINE labelOptic #-}

instance
  ( k ~ A_Lens,
    a ~ LineTruncF p,
    b ~ LineTruncF p
  ) =>
  LabelOptic "lineTrunc" k (ConsoleLoggingP p) (ConsoleLoggingP p) a b
  where
  labelOptic =
    lensVL
      $ \f (MkConsoleLoggingP a1 a2 a3 a4 a5) ->
        fmap
          (\b -> MkConsoleLoggingP a1 a2 b a4 a5)
          (f a3)
  {-# INLINE labelOptic #-}

instance
  ( k ~ A_Lens,
    a ~ ConfigPhaseF p ConsoleLogStripControl,
    b ~ ConfigPhaseF p ConsoleLogStripControl
  ) =>
  LabelOptic "stripControl" k (ConsoleLoggingP p) (ConsoleLoggingP p) a b
  where
  labelOptic =
    lensVL
      $ \f (MkConsoleLoggingP a1 a2 a3 a4 a5) ->
        fmap
          (\b -> MkConsoleLoggingP a1 a2 a3 b a5)
          (f a4)
  {-# INLINE labelOptic #-}

instance
  ( k ~ A_Lens,
    a ~ ConfigPhaseF p TimerFormat,
    b ~ ConfigPhaseF p TimerFormat
  ) =>
  LabelOptic "timerFormat" k (ConsoleLoggingP p) (ConsoleLoggingP p) a b
  where
  labelOptic =
    lensVL
      $ \f (MkConsoleLoggingP a1 a2 a3 a4 a5) ->
        fmap
          (\b -> MkConsoleLoggingP a1 a2 a3 a4 b)
          (f a5)
  {-# INLINE labelOptic #-}

type ConsoleLoggingArgs = ConsoleLoggingP ConfigPhaseArgs

type ConsoleLoggingToml = ConsoleLoggingP ConfigPhaseToml

type ConsoleLoggingMerged = ConsoleLoggingP ConfigPhaseMerged

type ConsoleLoggingEnv = ConsoleLoggingP ConfigPhaseEnv

deriving stock instance Eq (ConsoleLoggingP ConfigPhaseArgs)

deriving stock instance Show (ConsoleLoggingP ConfigPhaseArgs)

deriving stock instance Eq (ConsoleLoggingP ConfigPhaseToml)

deriving stock instance Show (ConsoleLoggingP ConfigPhaseToml)

deriving stock instance Eq (ConsoleLoggingP ConfigPhaseMerged)

deriving stock instance Show (ConsoleLoggingP ConfigPhaseMerged)

instance
  ( Default (SwitchF p ConsoleLogCmdSwitch),
    Default (ConfigPhaseMaybeF p (Truncation TruncCommandName)),
    Default (LineTruncF p),
    Default (ConfigPhaseF p ConsoleLogStripControl),
    Default (ConfigPhaseF p TimerFormat)
  ) =>
  Default (ConsoleLoggingP p)
  where
  def =
    MkConsoleLoggingP
      { commandLogging = def,
        commandNameTrunc = def,
        lineTrunc = def,
        stripControl = def,
        timerFormat = def
      }

-- | Merges args and toml configs.
mergeConsoleLogging ::
  ( HasCallStack,
    Terminal :> es
  ) =>
  ConsoleLoggingArgs ->
  Maybe ConsoleLoggingToml ->
  Eff es ConsoleLoggingMerged
mergeConsoleLogging args mToml = do
  lineTrunc <-
    configToLineTrunc $ (args ^. #lineTrunc) <>? (toml ^. #lineTrunc)

  pure
    $ MkConsoleLoggingP
      { commandLogging =
          WD.fromDefault
            ( review #boolIso
                <$> argsCommandLogging
                <>? (toml ^. #commandLogging)
            ),
        commandNameTrunc = (args ^. #commandNameTrunc) <?>? (toml ^. #commandNameTrunc),
        lineTrunc,
        stripControl =
          (args ^. #stripControl) <.>? (toml ^. #stripControl),
        timerFormat =
          (args ^. #timerFormat) <.>? (toml ^. #timerFormat)
      }
  where
    -- Convert WithDisabled () -> WithDisabled Bool for below operation.
    argsCommandLogging :: WithDisabled Bool
    argsCommandLogging = args ^. #commandLogging $> True

    toml = fromMaybe def mToml

instance DecodeTOML ConsoleLoggingToml where
  tomlDecoder =
    MkConsoleLoggingP
      <$> decodeCommandLogging
      <*> decodeCommandNameTrunc
      <*> decodeLineTrunc
      <*> decodeStripControl
      <*> decodeTimerFormat

decodeCommandLogging :: Decoder (Maybe Bool)
decodeCommandLogging = getFieldOptWith tomlDecoder "command"

decodeStripControl :: Decoder (Maybe ConsoleLogStripControl)
decodeStripControl = getFieldOptWith tomlDecoder "strip-control"

decodeTimerFormat :: Decoder (Maybe TimerFormat)
decodeTimerFormat = getFieldOptWith tomlDecoder "timer-format"

toEnv :: ConsoleLoggingMerged -> ConsoleLoggingEnv
toEnv merged =
  MkConsoleLoggingP
    { commandLogging = merged ^. #commandLogging,
      commandNameTrunc = merged ^. #commandNameTrunc,
      lineTrunc = merged ^. #lineTrunc,
      stripControl = merged ^. #stripControl,
      timerFormat = merged ^. #timerFormat
    }
