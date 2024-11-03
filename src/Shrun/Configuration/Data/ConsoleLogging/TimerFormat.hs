-- | Provides the 'TimerFormat' type.
module Shrun.Configuration.Data.ConsoleLogging.TimerFormat
  ( -- * Type
    TimerFormat (..),

    -- * Parsing
    parseTimerFormat,
    timerFormatStr,

    -- * Formatting
    formatRelativeTime,
    formatSeconds,
  )
where

import Data.Text qualified as T
import Data.Time.Relative
  ( Format (MkFormat),
    FormatStyle (FormatStyleDigital, FormatStyleProse),
    FormatVerbosity
      ( FormatVerbosityCompact,
        FormatVerbosityFull
      ),
    RelativeTime,
  )
import Data.Time.Relative qualified as RT
import Shrun.Configuration.Default (Default (def))
import Shrun.Data.Text (UnlinedText (UnsafeUnlinedText))
import Shrun.Prelude
import Shrun.Utils qualified as Utils

-- | Determines how to format the timer.
data TimerFormat
  = DigitalCompact
  | DigitalFull
  | ProseCompact
  | ProseFull
  deriving stock (Eq, Show)

instance DecodeTOML TimerFormat where
  tomlDecoder = parseTimerFormat tomlDecoder

instance Default TimerFormat where
  def = ProseCompact

-- | Parse timer format.
parseTimerFormat :: (MonadFail m) => m Text -> m TimerFormat
parseTimerFormat getTxt =
  getTxt >>= \case
    "digital_compact" -> pure DigitalCompact
    "digital_full" -> pure DigitalFull
    "prose_compact" -> pure ProseCompact
    "prose_full" -> pure ProseFull
    bad ->
      fail
        $ Utils.fmtUnrecognizedError
          "timer format"
          timerFormatStr
          (unpack bad)

-- | Available 'TimerFormat' strings.
timerFormatStr :: (IsString a) => a
timerFormatStr = "(digital_compact | digital_full | prose_compact | prose_full)"

-- NOTE: Time formatting does not include newlines, so using UnsafeUnlinedText
-- is safe. We use the constructor rather than unsafeUnlinedText.

-- | Formats a relative time.
formatRelativeTime :: TimerFormat -> RelativeTime -> UnlinedText
formatRelativeTime fmt =
  UnsafeUnlinedText
    . T.pack
    . RT.formatRelativeTime (toRelativeTimeFormat fmt)

-- | Formats a relative time seconds.
formatSeconds :: TimerFormat -> Natural -> UnlinedText
formatSeconds fmt =
  UnsafeUnlinedText
    . T.pack
    . RT.formatSeconds (toRelativeTimeFormat fmt)

toRelativeTimeFormat :: TimerFormat -> Format
toRelativeTimeFormat DigitalCompact = MkFormat FormatStyleDigital FormatVerbosityCompact
toRelativeTimeFormat DigitalFull = MkFormat FormatStyleDigital FormatVerbosityFull
toRelativeTimeFormat ProseCompact = MkFormat FormatStyleProse FormatVerbosityCompact
toRelativeTimeFormat ProseFull = MkFormat FormatStyleProse FormatVerbosityFull
