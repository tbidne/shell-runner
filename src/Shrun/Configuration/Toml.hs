{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides the 'TomlConfig' type.
--
-- @since 0.5
module Shrun.Configuration.Toml
  ( TomlConfig (..),
    defaultTomlConfig,
    CmdLoggingToml (..),
    FileLoggingToml (..),
    mergeConfig,
  )
where

import Optics.Core (NoIx)
import Shrun.Configuration.Args (Args (..), FileMode (..), FileSizeMode (..))
import Shrun.Configuration.Env.Types
  ( CmdDisplay,
    LineTruncation,
    StripControl,
    TruncRegion (..),
    Truncation,
  )
import Shrun.Data.FilePathDefault (FilePathDefault)
import Shrun.Data.Legend (KeyVal)
import Shrun.Data.Timeout (Timeout)
import Shrun.Prelude

-- | Logging that applies to command logs.
--
-- @since 0.6
data CmdLoggingToml = MkCmdLoggingToml
  { -- | @since 0.6
    stripControl :: !(Maybe StripControl),
    -- | @since 0.6
    lineTrunc :: !(Maybe LineTruncation)
  }
  deriving stock
    ( -- | @since 0.6
      Eq,
      -- | @since 0.6
      Show
    )

-- | @since 0.6
instance DecodeTOML CmdLoggingToml where
  tomlDecoder =
    MkCmdLoggingToml
      <$> decodeStripControl
      <*> decodeCmdLineTrunc

-- | @since 0.6
data FileLoggingToml = MkFileLoggingToml
  { -- | @since 0.6
    path :: !FilePathDefault,
    -- | @since 0.6
    stripControl :: !(Maybe StripControl),
    -- | @since 0.6
    mode :: !(Maybe FileMode),
    -- | @since 0.6
    sizeMode :: !(Maybe FileSizeMode)
  }
  deriving stock
    ( -- | @since 0.6
      Eq,
      -- | @since 0.6
      Show
    )

-- | @since 0.6
instance DecodeTOML FileLoggingToml where
  tomlDecoder =
    MkFileLoggingToml
      <$> decodeFileLogging
      <*> decodeFileLogStripControl
      <*> decodeFileLogMode
      <*> decodeFileLogSizeMode

-- | 'TomlConfig' refers to TomlConfiguration we retrieve from the toml TomlConfig file.
--
-- @since 0.5
data TomlConfig = MkTomlConfig
  { -- | Timeout.
    --
    -- @since 0.5
    timeout :: !(Maybe Timeout),
    -- | Whether to display the command (key) names or the commands
    -- themselves.
    --
    -- @since 0.5
    cmdDisplay :: !(Maybe CmdDisplay),
    -- | Truncates command names in the logs.
    --
    -- @since 0.6
    cmdNameTrunc :: !(Maybe (Truncation 'TCmdName)),
    -- | Whether to log commands.
    --
    -- @since 0.6
    cmdLogging :: !(Maybe CmdLoggingToml),
    -- | Optional file logging. If enabled, holds the path to the file
    -- and the log queue.
    --
    -- @since 0.6
    fileLogging :: !(Maybe FileLoggingToml),
    -- | Legend text containing command aliases.
    --
    -- @since 0.5
    legend :: !(Maybe (List KeyVal))
  }
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Show
    )

-- | Returns an empty 'TomlConfig'.
--
-- @since 0.7
defaultTomlConfig :: TomlConfig
defaultTomlConfig = MkTomlConfig Nothing Nothing Nothing Nothing Nothing Nothing

-- | @since 0.5
instance DecodeTOML TomlConfig where
  tomlDecoder =
    MkTomlConfig
      <$> decodeTimeout
      <*> decodeCmdDisplay
      <*> decodeCmdNameTrunc
      <*> getFieldOptWith tomlDecoder "cmd-log"
      <*> getFieldOptWith tomlDecoder "file-log"
      <*> decodeLegend

-- | @since 0.5
decodeTimeout :: Decoder (Maybe Timeout)
decodeTimeout = getFieldOptWith tomlDecoder "timeout"

-- | @since 0.5
decodeFileLogging :: Decoder FilePathDefault
decodeFileLogging = getFieldWith tomlDecoder "path"

-- | @since 0.5
decodeFileLogMode :: Decoder (Maybe FileMode)
decodeFileLogMode = getFieldOptWith tomlDecoder "mode"

-- | @since 0.5
decodeCmdDisplay :: Decoder (Maybe CmdDisplay)
decodeCmdDisplay = getFieldOptWith tomlDecoder "key-hide"

-- | @since 0.5
decodeCmdNameTrunc :: Decoder (Maybe (Truncation 'TCmdName))
decodeCmdNameTrunc = getFieldOptWith tomlDecoder "cmd-name-trunc"

-- | @since 0.5
decodeCmdLineTrunc :: Decoder (Maybe LineTruncation)
decodeCmdLineTrunc = getFieldOptWith tomlDecoder "line-trunc"

-- | @since 0.5
decodeStripControl :: Decoder (Maybe StripControl)
decodeStripControl = getFieldOptWith tomlDecoder "strip-control"

-- | @since 0.5
decodeFileLogStripControl :: Decoder (Maybe StripControl)
decodeFileLogStripControl = getFieldOptWith tomlDecoder "strip-control"

-- | @since 0.5
decodeFileLogSizeMode :: Decoder (Maybe FileSizeMode)
decodeFileLogSizeMode = getFieldOptWith tomlDecoder "size-mode"

-- | @since 0.5
decodeLegend :: Decoder (Maybe (List KeyVal))
decodeLegend = getFieldOptWith tomlDecoder "legend"

-- | @since 0.6
makeFieldLabelsNoPrefix ''CmdLoggingToml

-- | @since 0.6
makeFieldLabelsNoPrefix ''FileLoggingToml

-- | @since 0.5
makeFieldLabelsNoPrefix ''TomlConfig

-- | Merges an 'Args' and 'TomlConfig' together to produce a single config.
-- In general, if both configurations specify a value, the CLI 'Args'
-- takes precedence.
--
-- @since 0.1
mergeConfig :: Args -> TomlConfig -> TomlConfig
mergeConfig args tomlConfig =
  MkTomlConfig
    { timeout = combine #timeout #timeout,
      cmdDisplay = combine #cmdDisplay #cmdDisplay,
      cmdNameTrunc = combine #cmdNameTrunc #cmdNameTrunc,
      cmdLogging =
        combineCmdLogging argsCmdLogging (tomlConfig ^. #cmdLogging),
      fileLogging =
        combineFileLogging argsFileLogging (tomlConfig ^. #fileLogging),
      legend = tomlConfig ^. #legend
    }
  where
    combine ::
      ( Alternative f,
        Is k A_Getter
      ) =>
      Optic' k NoIx Args (f b) ->
      Optic' k NoIx TomlConfig (f b) ->
      f b
    combine argsGetter tomlGetter =
      args ^. argsGetter <|> tomlConfig ^. tomlGetter

    argsCmdLogging =
      ( args ^. #cmdLogging,
        args ^. #cmdLogStripControl,
        args ^. #cmdLogLineTrunc
      )
    argsFileLogging =
      ( args ^. #fileLogging,
        args ^. #fileLogStripControl,
        args ^. #fileLogMode,
        args ^. #fileLogSizeMode
      )

combineCmdLogging ::
  -- | Args
  (Maybe Bool, Maybe StripControl, Maybe LineTruncation) ->
  -- | Toml
  Maybe CmdLoggingToml ->
  -- | Result
  Maybe CmdLoggingToml
-- 1. If neither CLI nor toml specifies cmd logging, return no logging
combineCmdLogging (Just False, _, _) Nothing = Nothing
combineCmdLogging (Nothing, _, _) Nothing = Nothing
-- 2. If only the CLI specifies cmd logging, use its config
combineCmdLogging (Just True, sc, lt) Nothing = Just $ MkCmdLoggingToml sc lt
-- 3. If toml specifies cmd logging, combine args, favoring CLI as usual
combineCmdLogging (_, mStripControl, mlineTrunc) (Just toml) =
  Just $
    MkCmdLoggingToml
      { stripControl = mStripControl <|> toml ^. #stripControl,
        lineTrunc = mlineTrunc <|> toml ^. #lineTrunc
      }

combineFileLogging ::
  -- | Args
  (Maybe FilePathDefault, Maybe StripControl, Maybe FileMode, Maybe FileSizeMode) ->
  -- | Toml
  Maybe FileLoggingToml ->
  -- | Result
  Maybe FileLoggingToml
-- 1. If neither CLI nor toml specifies file logging, return no logging
combineFileLogging (Nothing, _, _, _) Nothing = Nothing
-- 2. If only the CLI specifies file logging, use its config
combineFileLogging (Just f, sc, m, sm) Nothing = Just $ MkFileLoggingToml f sc m sm
-- 3. If toml specifies file logging, combine args, favoring CLI as usual
combineFileLogging (mpath, mStripControl, mMode, mSizeMode) (Just toml) =
  Just $
    MkFileLoggingToml
      { path = fromMaybe (toml ^. #path) mpath,
        stripControl = mStripControl <|> toml ^. #stripControl,
        mode = mMode <|> toml ^. #mode,
        sizeMode = mSizeMode <|> toml ^. #sizeMode
      }
