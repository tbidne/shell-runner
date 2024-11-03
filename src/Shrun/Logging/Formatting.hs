-- | Provides 'Log' formatting functionality.
module Shrun.Logging.Formatting
  ( -- * High-level
    formatConsoleLog,
    formatFileLog,

    -- * Low-level
    logToColor,
    logToPrefix,
    levelToColor,
    levelToPrefix,

    -- ** Utils
    formatCommand,
    concatWithLineTrunc,
    displayCmd,
    stripChars,
    brackets,
    formatCommandText,
  )
where

import Data.Text qualified as T
import Effectful.Time.Dynamic (getSystemTimeString)
import Shrun.Configuration.Data.CommonLogging.KeyHideSwitch
  ( KeyHideSwitch (KeyHideOff),
  )
import Shrun.Configuration.Data.ConsoleLogging (ConsoleLoggingEnv)
import Shrun.Configuration.Data.FileLogging (FileLoggingEnv)
import Shrun.Configuration.Data.StripControl
  ( StripControl (StripControlAll, StripControlNone, StripControlSmart),
  )
import Shrun.Configuration.Data.Truncation
  ( TruncRegion
      ( TruncCommandName,
        TruncLine
      ),
    Truncation (MkTruncation),
  )
import Shrun.Data.Command (CommandP (MkCommandP), CommandP1)
import Shrun.Data.Text (UnlinedText)
import Shrun.Data.Text qualified as ShrunText
import Shrun.Logging.Types
  ( Log,
    LogLevel
      ( LevelCommand,
        LevelError,
        LevelFatal,
        LevelFinished,
        LevelSuccess,
        LevelTimer,
        LevelWarn
      ),
  )
import Shrun.Logging.Types.Internal
  ( ConsoleLog (UnsafeConsoleLog),
    FileLog (UnsafeFileLog),
  )
import Shrun.Prelude
import Shrun.Utils ((∸))
import Shrun.Utils qualified as Utils
import System.Console.Pretty (Color (Blue, Cyan, Green, Red, White, Yellow))
import System.Console.Pretty qualified as P

-- | Formats a log to be printed to the console.
formatConsoleLog ::
  KeyHideSwitch ->
  ConsoleLoggingEnv ->
  Log ->
  ConsoleLog
formatConsoleLog keyHide consoleLogging log = UnsafeConsoleLog (colorize line)
  where
    -- NOTE: We want colorize on the outside for two reasons:
    --
    -- 1. Truncation calculation should not take colorization into account,
    --    as chars are invisible.
    -- 2. Having colorization _inside_ can accidentally cause the "end color"
    --    chars to be stripped, leading to bugs where colorizing bleeds.
    --
    -- This 2nd point is likely the cause for some "color bleeding" that was
    -- occasionally noticed.
    colorize = P.color $ logToColor log

    line =
      coreFormatting
        ((,Nothing) <$> consoleLogging ^. #lineTrunc)
        (consoleLogging ^. #commandNameTrunc)
        True
        (consoleLogging ^. #stripControl)
        keyHide
        log

maybeApply :: (a -> b -> b) -> Maybe a -> b -> b
maybeApply = maybe id

-- | Formats a 'Log' into a 'FileLog'. Applies prefix and timestamp.
formatFileLog ::
  ( HasCallStack,
    Time :> es
  ) =>
  KeyHideSwitch ->
  FileLoggingEnv ->
  Log ->
  Eff es FileLog
formatFileLog keyHide fileLogging log = do
  currTime <- getSystemTimeString
  let timestamp = brackets False (pack currTime)
      timestampLen = T.length timestamp

      line =
        coreFormatting
          ((,Just timestampLen) <$> fileLogging ^. #lineTrunc)
          (fileLogging ^. #commandNameTrunc)
          False
          (fileLogging ^. #stripControl)
          keyHide
          log

      withTimestamp =
        mconcat
          [ timestamp,
            line,
            "\n"
          ]

  pure $ UnsafeFileLog withTimestamp

-- | Core formatting, shared by console and file logs. Basic idea:
--
-- 1. If the log contains a command, it is formatted according to
--    'formatCommand' and command name truncation.
--
-- 2. The message is stripped of control chars according to strip control.
--
-- 3. Line truncation is applied if applicable. Note this applies only to
--    the stripped message. The prefix (e.g. level label, timestamp, command
--    name) are always present, though they __do__ count towards the
--    truncation count. I.e. if the prefixes add up to 10 chars, and the
--    line truncation is 15, then we only have 5 chars for the message before
--    truncation kicks in.
coreFormatting ::
  -- | Optional line truncation. If we have some line truncation then there
  -- is a further optional "prefix length". This is so that file logging
  -- can pass in the timestamp length so it is taken into account
  -- (command logging has no special prefix besides ANSI codes, which is
  -- ignored).
  Maybe (Truncation TruncLine, Maybe Int) ->
  -- | Optional cmd name truncation
  Maybe (Truncation TruncCommandName) ->
  -- | If true, strips leading whitespace. This is so that file logging
  -- can preserve leading whitespace (so that the file output retains
  -- potential alignment; alignment is irrelevant to console logs).
  --
  -- This is for leading only, not trailing, as the latter is irrelevant to
  -- formatting, and we may want trailing whitespace if we ever concat two
  -- logs together.
  Bool ->
  -- | Strip control
  StripControl t ->
  -- | Key hide
  KeyHideSwitch ->
  -- | Log to format
  Log ->
  Text
coreFormatting
  mLineTrunc
  mCommandNameTrunc
  stripLeading
  stripControl
  keyHide
  log =
    concatWithLineTrunc mLineTrunc prefix (msgStripped ^. #unUnlinedText)
    where
      -- prefix is something like "[Success] " or "[Command][some cmd] ".
      -- Notice this does not include ANSI codes or a timestamp.
      prefix = case log ^. #cmd of
        Nothing -> brackets True logPrefix
        Just cmd ->
          let cmd' =
                formatCommand
                  keyHide
                  mCommandNameTrunc
                  cmd
           in mconcat
                [ brackets False logPrefix,
                  cmd' ^. #unUnlinedText
                ]

      msgStripControlled = stripChars (log ^. #msg) stripControl
      msgStripped =
        if stripLeading
          then ShrunText.reallyUnsafeMap T.stripStart msgStripControlled
          else msgStripControlled

      logPrefix = logToPrefix log

formatCommand ::
  KeyHideSwitch ->
  Maybe (Truncation TruncCommandName) ->
  CommandP1 ->
  UnlinedText
formatCommand keyHide commandNameTrunc com =
  ShrunText.reallyUnsafeMap (brackets True . truncateNameFn) cmdName
  where
    -- Get cmd name to display. Always strip control sequences. Futhermore,
    -- strip leading/trailing whitespace.
    cmdName = displayCmd com keyHide

    -- truncate cmd/name if necessary
    truncateNameFn =
      maybeApply
        Utils.truncateIfNeeded
        (commandNameTrunc ^? (_Just % #unTruncation))

-- | Replace newlines with whitespace before stripping, so any strings
-- separated by newlines do not get smashed together.
formatCommandText :: Text -> UnlinedText
formatCommandText =
  ShrunText.reallyUnsafeMap T.strip
    . Utils.stripControlAll
    . ShrunText.fromTextReplace

-- | Combines a prefix @p@ and msg @m@ with possible line truncation. If no
-- truncation is given then concatWithLineTrunc is equivalent to @p <> m@.
-- If we are given some line truncation @l@, then we derive
--
-- @
--    k := l - prefix_len -- k is clamped to zero
-- @
--
-- and return
--
-- @
--    prefix <> t'
-- @
--
-- where @t'@ is @t@ truncated to @k@ chars. Notice the prefix is always
-- included untarnished.
concatWithLineTrunc ::
  Maybe (Truncation TruncLine, Maybe Int) ->
  Text ->
  Text ->
  Text
concatWithLineTrunc Nothing prefix msg = prefix <> msg
concatWithLineTrunc (Just (MkTruncation lineTrunc, mPrefixLen)) prefix msg =
  prefix <> Utils.truncateIfNeeded lineTrunc' msg
  where
    lineTrunc' =
      lineTrunc ∸ (prefixLen + T.length prefix)

    prefixLen = fromMaybe 0 mPrefixLen

-- | Pretty show for 'Command'. If the command has a key, and 'KeyHideSwitch' is
-- 'KeyHideOff' then we return the key. Otherwise we return the command itself.
--
-- >>> displayCmd (MkCommandP Nothing "some long command") KeyHideOn
-- "some long command"
--
-- >>> displayCmd (MkCommandP Nothing "some long command") KeyHideOff
-- "some long command"
--
-- >>> displayCmd (MkCommandP (Just "long") "some long command") KeyHideOn
-- "some long command"
--
-- >>> displayCmd (MkCommandP (Just "long") "some long command") KeyHideOff
-- "long"
displayCmd :: CommandP1 -> KeyHideSwitch -> UnlinedText
displayCmd (MkCommandP (Just key) _) KeyHideOff = formatCommandText key
displayCmd (MkCommandP _ cmd) _ = formatCommandText cmd

-- | Applies the given 'StripControl' to the 'Text'.
--
-- * 'StripControlAll': All control chars.
-- * 'StripControlSmart': Ansi control chars.
-- * 'StripControlNone': Nothing.
stripChars :: UnlinedText -> StripControl t -> UnlinedText
stripChars txt = \case
  StripControlAll -> Utils.stripControlAll txt
  StripControlNone -> txt
  StripControlSmart -> Utils.stripControlSmart txt
{-# INLINE stripChars #-}

-- | Surrounds text with brackets, appending a space if the boolean is 'True'.
--
-- ==== __Examples__
--
-- >>> brackets False "text"
-- "[text]"
--
-- >>> brackets True "text"
-- "[text] "
brackets :: Bool -> Text -> Text
brackets False s = "[" <> s <> "]"
brackets True s = "[" <> s <> "] "

-- | Transforms log to a color based on its 'LogLevel'.
logToColor :: Log -> Color
logToColor = levelToColor . view #lvl

-- | Transforms log to a prefix based on its 'LogLevel'.
logToPrefix :: Log -> Text
logToPrefix = levelToPrefix . view #lvl

-- | Maps 'LogLevel' to 'Color'.
levelToColor :: LogLevel -> Color
levelToColor LevelCommand = White
levelToColor LevelFinished = Blue
levelToColor LevelTimer = Cyan
levelToColor LevelSuccess = Green
levelToColor LevelWarn = Yellow
levelToColor LevelError = Red
levelToColor LevelFatal = Red

-- | Maps 'LogLevel' to \'Prefix\'.
levelToPrefix :: LogLevel -> Text
levelToPrefix LevelCommand = "Command"
levelToPrefix LevelFinished = "Finished"
levelToPrefix LevelTimer = "Timer"
levelToPrefix LevelSuccess = "Success"
levelToPrefix LevelWarn = "Warn"
levelToPrefix LevelError = "Error"
levelToPrefix LevelFatal = "Fatal"
