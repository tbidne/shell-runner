{-# LANGUAGE TemplateHaskell #-}

-- | Provides functionality for parsing command line arguments.
--
-- @since 0.1.0.0
module ShellRun.Args
  ( Args (..),
    ALineTruncation (..),
    defaultArgs,
    parserInfoArgs,
  )
where

import Control.Applicative qualified as App
import Data.List qualified as L
import Data.List.NonEmpty qualified as NE
import Data.String (IsString (..), String)
import Data.Text qualified as T
import Data.Version.Package qualified as PV
import Development.GitRev qualified as GitRev
import Options.Applicative (ParseError (..), Parser, ParserInfo (..), ReadM)
import Options.Applicative qualified as OApp
import Options.Applicative.Help.Chunk (Chunk (..))
import Options.Applicative.Types (ArgPolicy (..))
import ShellRun.Data.InfNum (PosInfNum (..))
import ShellRun.Data.NonEmptySeq (NonEmptySeq (..))
import ShellRun.Data.NonEmptySeq qualified as NESeq
import ShellRun.Data.TimeRep (TimeRep (..))
import ShellRun.Data.TimeRep qualified as TimeRep
import ShellRun.Data.Timeout (Timeout (..))
import ShellRun.Env.Types
  ( CommandDisplay (..),
    CommandLogging (..),
    Truncation (..),
    TruncationArea (..),
  )
import ShellRun.Prelude
import Text.Megaparsec (Parsec)
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Char qualified as MPC
import Text.Read qualified as TR

-- | Type for parsing command line args.
--
-- @since 0.1.0.0
data Args = MkArgs
  { -- | Whether to log commands.
    --
    -- @since 0.1.0.0
    aCommandLogging :: CommandLogging,
    -- | Optional path to log file..
    --
    -- @since 0.1.0.0
    aFileLogging :: Maybe FilePath,
    -- | Whether to display command by (key) name or command.
    --
    -- @since 0.1.0.0
    aCommandDisplay :: CommandDisplay,
    -- | Optional legend file.
    --
    -- @since 0.1.0.0
    aLegend :: Maybe FilePath,
    -- | Timeout.
    --
    -- @since 0.1.0.0
    aTimeout :: Timeout,
    -- | The max number of command characters to display in the logs.
    --
    -- @since 0.1.0.0
    aCmdTruncation :: Truncation 'TCommand,
    -- | The max number of line characters to display in the logs.
    --
    -- @since 0.1.0.0
    aLineTruncation :: ALineTruncation,
    -- | List of commands.
    --
    -- @since 0.1.0.0
    aCommands :: NonEmptySeq Text
  }
  deriving
    ( -- | @since 0.1.0.0
      Eq,
      -- | @since 0.1.0.0
      Show
    )

-- | Determines command log line truncation behavior. We need to separate
-- type from 'Truncation' to add a third option, to detect the terminal size
-- automatically.
--
-- @since 0.1.0.0
data ALineTruncation
  = -- | @since 0.1.0.0
    Undetected (Truncation 'TLine)
  | -- | @since 0.1.0.0
    Detected
  deriving
    ( -- | @since 0.1.0.0
      Eq,
      -- | @since 0.1.0.0
      Show
    )

-- | @since 0.1.0.0
instance Semigroup ALineTruncation where
  Undetected x <> Undetected y = Undetected (x <> y)
  _ <> _ = Detected

-- | @since 0.1.0.0
instance Monoid ALineTruncation where
  mempty = Undetected mempty

-- | Default configuration.
--
-- ==== __Examples__
-- >>> defaultArgs (NESeq.singleton "ls")
-- MkArgs {aCommandLogging = Disabled, aFileLogging = Nothing, aCommandDisplay = ShowCommand, aLegend = Nothing, aTimeout = MkTimeout {unTimeout = PPosInf}, aCmdTruncation = MkTruncation {unTruncation = PPosInf}, aLineTruncation = Undetected (MkTruncation {unTruncation = PPosInf}), aCommands = "ls" :|^ fromList []}
--
-- @since 0.1.0.0
defaultArgs :: NonEmptySeq Text -> Args
defaultArgs cmds =
  MkArgs
    { aCommandLogging = mempty,
      aFileLogging = empty,
      aCommandDisplay = mempty,
      aLegend = empty,
      aTimeout = mempty,
      aCmdTruncation = mempty,
      aLineTruncation = mempty,
      aCommands = cmds
    }

-- | 'ParserInfo' type for parsing 'Args'.
--
-- @since 0.1.0.0
parserInfoArgs :: ParserInfo Args
parserInfoArgs =
  ParserInfo
    { infoParser = argsParser,
      infoFullDesc = True,
      infoProgDesc = Chunk desc,
      infoHeader = Chunk header,
      infoFooter = Chunk footer,
      infoFailureCode = 1,
      infoPolicy = Intersperse
    }
  where
    header = Just "Shell-Run: A tool for running shell commands ergonomically."
    footer = Just $ fromString versNum
    desc =
      Just $
        "\nShell-Run runs shell commands concurrently. In addition to "
          <> "providing basic timing and logging functionality, we also provide "
          <> "the ability to pass in a legend file that can be used to define "
          <> "aliases for commands. See github.com/tbidne/shell-run#README for "
          <> "full documentation."

argsParser :: Parser Args
argsParser =
  MkArgs
    <$> commandLoggingParser
    <*> fileLoggingParser
    <*> commandDisplayParser
    <*> legendParser
    <*> timeoutParser
    <*> cmdTruncationParser
    <*> lineTruncationParser
    <*> commandsParser
      <**> OApp.helper
      <**> version

version :: Parser (a -> a)
version = OApp.infoOption txt (OApp.long "version" <> OApp.short 'v')
  where
    txt =
      L.intercalate
        "\n"
        [ "Shell-Run",
          versNum,
          "Revision: " <> $(GitRev.gitHash),
          "Date: " <> $(GitRev.gitCommitDate)
        ]

versNum :: List Char
versNum = "Version: " <> $$(PV.packageVersionStringTH "shell-run.cabal")

legendParser :: Parser (Maybe FilePath)
legendParser =
  App.optional
    ( OApp.strOption
        ( OApp.long "legend"
            <> OApp.short 'l'
            <> OApp.help legendHelp
            <> OApp.metavar "PATH"
        )
    )
  where
    legendHelp =
      "Path to legend file, used for translating commands."
        <> " Key/value pairs have the form `key=cmd1,,cmd2,,...`"
        <> ", i.e., keys can refer to multiple commands and refer to"
        <> " other keys recursively. Lines starting with `#` are"
        <> " considered comments and ignored."

timeoutParser :: Parser Timeout
timeoutParser =
  OApp.option
    (readTimeSeconds <|> readTimeStr)
    ( OApp.value (MkTimeout PPosInf)
        <> OApp.long "timeout"
        <> OApp.short 't'
        <> OApp.help help
        <> OApp.metavar "NATURAL"
    )
  where
    help =
      "Non-negative integer setting a timeout. Can either be a raw number "
        <> "(interpreted as seconds), or a \"time string\" e.g. 1d2h3m4s or "
        <> "2h3s. Defaults to no timeout."

readTimeSeconds :: ReadM Timeout
readTimeSeconds = MkTimeout . PFin <$> OApp.auto

readTimeStr :: ReadM Timeout
readTimeStr = do
  v :: String <- OApp.str
  case MP.parse parseTimeRep "ShellRun.Args" v of
    Left _ ->
      OApp.readerAbort $
        ErrorMsg $
          "Wanted time string e.g. 1d2h3m4s. Received: " <> v
    Right timeRep ->
      let timeout = MkTimeout $ PFin $ TimeRep.toSeconds timeRep
       in pure timeout

cmdTruncationParser :: Parser (Truncation 'TCommand)
cmdTruncationParser =
  OApp.option
    readTruncation
    ( OApp.value (MkTruncation PPosInf)
        <> OApp.long "cmd-name-truncate"
        <> OApp.short 'x'
        <> OApp.help help
        <> OApp.metavar "NATURAL"
    )
  where
    help =
      "Non-negative integer that limits the length of commands/key-names "
        <> "in the console logs. Defaults to no truncation. This affects "
        <> "everywhere the command/key-name shows up (i.e. in command logs or "
        <> "final success/error message), it does not affect file logs "
        <> "created via --file-log."

lineTruncationParser :: Parser ALineTruncation
lineTruncationParser =
  OApp.option
    (defRead <|> readDetectTruncation)
    ( OApp.value defValue
        <> OApp.long "cmd-line-truncate"
        <> OApp.short 'y'
        <> OApp.help help
        <> OApp.metavar "NATURAL or detect"
    )
  where
    defValue = Undetected (MkTruncation PPosInf)
    defRead = Undetected <$> readTruncation
    help =
      "Non-negative integer that limits the length of logs "
        <> "produced via --cmd-log in the console logs. Can also be the "
        <> "string literal 'detect' (no quotes), to detect the terminal "
        <> "size automatically. Defaults to no truncation. This does not "
        <> "affect file logs with --file-log."

readTruncation :: ReadM (Truncation a)
readTruncation = MkTruncation . PFin <$> OApp.auto

readDetectTruncation :: ReadM ALineTruncation
readDetectTruncation = do
  s <- OApp.str
  if T.toCaseFold s == "detect"
    then pure Detected
    else
      OApp.readerAbort $
        ErrorMsg $ "Unrecognized truncation option:" <> T.unpack s

fileLoggingParser :: Parser (Maybe FilePath)
fileLoggingParser =
  App.optional
    ( OApp.strOption
        ( OApp.long "file-log"
            <> OApp.short 'f'
            <> OApp.help help
            <> OApp.metavar "PATH"
        )
    )
  where
    help =
      "If a path is supplied, all logs will additionally be written to the "
        <> "supplied file. Furthermore, command logs will be written to the "
        <> "file irrespective of --cmd-logging. Console logging is "
        <> "unaffected. This can be useful for investigating command failures."

commandLoggingParser :: Parser CommandLogging
commandLoggingParser =
  OApp.flag
    Disabled
    Enabled
    ( OApp.short 'c'
        <> OApp.long "cmd-log"
        <> OApp.help help
    )
  where
    help = "Adds individual commands' logs (stdout+stderr) to console output."

commandDisplayParser :: Parser CommandDisplay
commandDisplayParser =
  OApp.flag
    ShowCommand
    ShowKey
    ( OApp.short 'k'
        <> OApp.long "key-show"
        <> OApp.help help
    )
  where
    help =
      "In console output, display key name over actual command if it "
        <> "exists."

commandsParser :: Parser (NonEmptySeq Text)
commandsParser =
  NESeq.unsafeFromNonEmpty
    <$> NE.some1
      ( T.pack
          <$> OApp.argument OApp.str (OApp.metavar "Commands...")
      )

type MParser = Parsec Text (List Char)

parseTimeRep :: MParser TimeRep
parseTimeRep =
  MkTimeRep
    <$> parseTimeOrZero 'd'
    <*> parseTimeOrZero 'h'
    <*> parseTimeOrZero 'm'
    <*> parseTimeOrZero 's'
    <* MP.eof

parseTimeOrZero :: Char -> MParser Natural
parseTimeOrZero c =
  -- Backtrack if we don't match
  MP.try (parseNNWithUnit c)
    <|> pure 0

parseNNWithUnit :: Char -> MParser Natural
parseNNWithUnit c = parseNonNegative <* MPC.char' c

parseNonNegative :: MParser Natural
parseNonNegative = do
  ds <- MP.some MPC.digitChar
  case TR.readMaybe ds of
    Nothing -> MP.customFailure $ "Could not parse natural: " <> showt ds
    Just n -> pure n
