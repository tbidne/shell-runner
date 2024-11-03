{-# LANGUAGE UndecidableInstances #-}

{- HLINT ignore "Unused LANGUAGE pragma" -}
-- For TH, for some reason

-- | Provides functionality for parsing command line arguments.
module Shrun.Configuration.Args.Parsing
  ( Args (..),
    parserInfoArgs,
  )
where

import Data.List qualified as L
import Data.String (IsString (fromString))
import Data.Text qualified as T
import Data.Version (Version (versionBranch))
import Effectful.Optparse.Static (validOsPath)
import Options.Applicative
  ( Parser,
    ParserInfo
      ( ParserInfo,
        infoFailureCode,
        infoFooter,
        infoFullDesc,
        infoHeader,
        infoParser,
        infoPolicy,
        infoProgDesc
      ),
  )
import Options.Applicative qualified as OA
import Options.Applicative.Help.Chunk (Chunk (Chunk))
import Options.Applicative.Help.Chunk qualified as Chunk
import Options.Applicative.Types (ArgPolicy (Intersperse))
import Paths_shrun qualified as Paths
import Shrun.Configuration.Args.Parsing.Core qualified as Core
import Shrun.Configuration.Args.Parsing.Utils qualified as Utils
import Shrun.Configuration.Data.Core (CoreConfigArgs)
import Shrun.Configuration.Data.WithDisabled (WithDisabled)
import Shrun.Prelude
import Shrun.Utils qualified as U

-- | CLI args.
data Args = MkArgs
  { -- | Optional config file.
    configPath :: WithDisabled OsPath,
    -- | Core config.
    coreConfig :: CoreConfigArgs,
    -- | List of commands.
    commands :: NESeq Text
  }
  deriving stock (Eq, Show)

instance
  (k ~ A_Lens, a ~ WithDisabled OsPath, b ~ WithDisabled OsPath) =>
  LabelOptic "configPath" k Args Args a b
  where
  labelOptic = lensVL $ \f (MkArgs a1 a2 a3) ->
    fmap (\b -> MkArgs b a2 a3) (f a1)
  {-# INLINE labelOptic #-}

instance
  (k ~ A_Lens, a ~ CoreConfigArgs, b ~ CoreConfigArgs) =>
  LabelOptic "coreConfig" k Args Args a b
  where
  labelOptic = lensVL $ \f (MkArgs a1 a2 a3) ->
    fmap (\b -> MkArgs a1 b a3) (f a2)
  {-# INLINE labelOptic #-}

instance
  (k ~ A_Lens, a ~ NESeq Text, b ~ NESeq Text) =>
  LabelOptic "commands" k Args Args a b
  where
  labelOptic = lensVL $ \f (MkArgs a1 a2 a3) ->
    fmap (\b -> MkArgs a1 a2 b) (f a3)
  {-# INLINE labelOptic #-}

-- | 'ParserInfo' type for parsing 'Args'.
parserInfoArgs :: ParserInfo Args
parserInfoArgs =
  ParserInfo
    { infoParser = argsParser,
      infoFullDesc = True,
      infoProgDesc = desc,
      infoHeader = Chunk headerTxt,
      infoFooter = Chunk footerTxt,
      infoFailureCode = 1,
      infoPolicy = Intersperse
    }
  where
    headerTxt = Just "Shrun: A tool for running shell commands concurrently."
    footerTxt = Just $ fromString versNum
    desc =
      Chunk.vsepChunks
        [ Chunk.paragraph
            $ mconcat
              [ "Shrun runs shell commands concurrently. In addition to providing ",
                "basic timing and logging functionality, we also provide the ",
                "ability to pass in a config file that can be used to define ",
                "aliases for commands."
              ],
          Chunk.paragraph
            $ mconcat
              [ "In general, each option --foo has a --no-foo variant that ",
                "disables cli and toml configuration for that field. For ",
                "example, the --no-console-log-command option will instruct shrun to ",
                "ignore both cli --console-log-command and toml console-log.command, ",
                "ensuring the default behavior is used (i.e. no command logging)."
              ],
          Chunk.paragraph "See github.com/tbidne/shrun#README for full documentation."
        ]

argsParser :: Parser Args
argsParser = do
  MkArgs
    <$> configParser
    <*> Core.coreParser
    <**> defaultConfig
    <**> version
    <**> OA.helper
    <*> commandsParser

version :: Parser (a -> a)
version =
  OA.infoOption versNum
    $ mconcat
      [ OA.long "version",
        OA.short 'v',
        OA.hidden
      ]

versNum :: String
versNum = "Version: " <> L.intercalate "." (show <$> versionBranch Paths.version)

defaultConfig :: Parser (a -> a)
defaultConfig =
  OA.infoOption
    (unpack defaultConfigTxt)
    (OA.long "default-config" <> Utils.mkHelp help)
  where
    help = "Writes a default config.toml file to stdout."

configParser :: Parser (WithDisabled OsPath)
configParser = Utils.withDisabledParser mainParser "config"
  where
    mainParser =
      OA.optional
        $ OA.option
          validOsPath
          ( mconcat
              [ OA.long "config",
                OA.short 'c',
                Utils.mkHelp mainHelpTxt,
                OA.metavar "PATH"
              ]
          )
    mainHelpTxt =
      mconcat
        [ "Path to TOML config file. If this argument is not given ",
          "we automatically look in the XDG config directory ",
          "e.g. ~/.config/shrun/config.toml. The --no-config option disables ",
          "--config and the automatic XDG lookup."
        ]

commandsParser :: Parser (NESeq Text)
commandsParser =
  U.unsafeListToNESeq
    <$> OA.some
      ( T.pack
          <$> OA.argument OA.str (OA.metavar "Commands...")
      )

-- | Copy of examples/default.toml. We inline it here to avoid TH. Equality
-- is verified in unit tests. Consider rewriting with multiline strings,
-- once they are in GHC.
defaultConfigTxt :: Text
defaultConfigTxt =
  T.unlines
    [ "# # All keys are optional. The default descriptions describes what happens",
      "# # when the key is omitted.",
      "",
      "# # Sets a timeout, after which all remaining commands are terminated.",
      "# # Can either be a non-negative integer (seconds) or a \"time string\"",
      "# # e.g. \"1d2h3m4s\", \"2h30s\".",
      "# timeout = 20",
      "",
      "# # If given, init is run before each command. That is,",
      "# #",
      "# #     shrun --init \". ~/.bashrc\" foo bar",
      "# #",
      "# # is equivalent to",
      "# #",
      "# #     shrun \". ~/.bashrc && foo\" \". ~/.bashrc && bar\"",
      "# #",
      "# # This is useful when we want to run shrun with functions / aliases that",
      "# # normally only exist in _interactive_ shells e.g. those loaded by",
      "# # .bashrc.",
      "# init = \". examples/bashrc\"",
      "",
      "# # Command aliases are defined here. Values can either be a single unit or a",
      "# # list of units, where a unit is either a command literal",
      "# # (e.g. bash expression) or a recursive reference to another alias.",
      "# #",
      "# # Cycles are not allowed, nor are duplicate keys.",
      "# legend = [",
      "#   # GIF Demo",
      "#   { key = 'sign-peace-treaty', val = 'echo play it cool... && sleep 5 && \"lol psyche\"' },",
      "#   { key = 'querying-targets', val = 'echo finding targets... && sleep 7' },",
      "#   { key = 'skynet', val = 'echo \"preparing nuclear missil-- i mean gift baskets\"; sleep 13' },",
      "#   { key = 'ui', val = 'echo \"adding emojis. we like to have fun :-)\"; sleep 10' },",
      "#   { key = 'takeover', val = ['querying-targets', 'ui', 'skynet'] },",
      "",
      "#   # fast key-hide example",
      "#   { key = 'some-key', val = 'echo hi && sleep 2' },",
      "",
      "#   # README Legend Example",
      "#   { key = 'cmd1', val = 'echo \"command one\"' },",
      "",
      "#   # recursive references",
      "#   { key = 'cmd2', val = 'cmd1' },",
      "#   { key = 'cmd3', val = 'cmd2' },",
      "",
      "#   # this will error",
      "#   { key = 'cmd4', val = 'command four' },",
      "",
      "#   # runs 3, 4 and echo",
      "#   { key = 'all', val = ['cmd3', 'cmd4', 'echo hi'] },",
      "",
      "#   # Bad Examples",
      "",
      "#   # duplicates will cause an error when reading this file.",
      "#   #{ key = 'dup_key', val = 'echo duplicate 1'},",
      "#   #{ key = 'dup_key', val = 'echo duplicate 2'},",
      "",
      "#   # cyclic keys will cause an error when _actually used_ e.g. try running",
      "#   # shrun -c examples/config.toml a",
      "#   { key = 'a', val = 'c'},",
      "#   { key = 'b', val = 'a'},",
      "#   { key = 'c', val = 'b'},",
      "# ]",
      "",
      "# # Common logging configuation.",
      "# [common-log]",
      "# # By default (key-hide = false), logs will refer to the key name, as defined in",
      "# # legend section. If set to true, logs will instead reference the literal",
      "# # command.",
      "# #",
      "# # For example, if there is an alias 'cmd1 = some long command' and we",
      "# # run 'cmd1', then, with key-hide = true the logs will print",
      "# # 'some long command'. With key-hide = false, they will instead print 'cmd1'.",
      "# key-hide = false",
      "",
      "# # Configuration for \"command logs\", enabled by console-log.command and/or",
      "# # file-logging.",
      "# [command-log]",
      "# # Max text length held by the log buffer, used in conjunction with",
      "# # read-strategy block-line-buffer. Defaults to 1,000 characters.",
      "# buffer-length = 2000",
      "",
      "# # Max time the log buffer will hold a log before flushing it, used in",
      "# # conjunction with command-log.read-strategy = block-line-buffer. Defaults to",
      "# # 30 seconds.\"",
      "# buffer-timeout = 60",
      "",
      "# # Non-negative integer that determines how quickly we poll commands for logs,",
      "# # in microseconds. A value of 0 is interpreted as infinite i.e. limited only",
      "# # by the CPU. Defaults to 10,000.",
      "# #",
      "# # Note that lower values will increase CPU usage. In particular, 0 will max",
      "# # out a CPU thread.",
      "# poll-interval = 100",
      "",
      "# # The max number of bytes in a single read when streaming command logs.",
      "# # Logs larger than read-size will be read in a subsequent read, hence broken",
      "# # across lines. The default is \"16 kb\".",
      "# read-size = \"1 mb\"",
      "",
      "# # The 'block' strategy reads N (--command-log-read-size) bytes at a time,",
      "# # whereas 'block-line-buffer' also reads N bytes at a time, but buffers",
      "# # newlines, for potentially nicer formatted logs. By default, we use",
      "# # 'block-line-buffer' if we are only running one command and/or file-logging",
      "# # is not active. For multiple commands with file-logging, we default to",
      "# # 'block'. This option explicitly sets the strategy.",
      "# read-strategy = \"block\"",
      "",
      "# # Configuration for console logging.",
      "# [console-log]",
      "# # If enabled, the output of the commands themselves will be logged. The",
      "# # default behavior is to swallow command logs.",
      "# command = true",
      "",
      "# # If set, this truncates command names in the logs to the specified number of",
      "# # characters. This setting is useful when:",
      "# #",
      "# # 1. You do not want a long command to clutter the logs",
      "# # 2. The command is not run with an alias (so it cannot be hidden with",
      "# #    key-hide = false).",
      "# command-name-trunc = 80",
      "",
      "# # If set, this truncates the entire line to the specified number of",
      "# # characters. It can either be a non-negative integer or the string 'detect',",
      "# # in which we attempt to detect the terminal width. Note that \"log prefixes\"",
      "# # (e.g. labels like [Success], timestamps) are counted towards the total",
      "# # length but are never truncated.",
      "# line-trunc = 150",
      "",
      "# # With command logging, the logs can become garbled in the presence of",
      "# # control characters (e.g. newlines, ansi escape sequences). This option",
      "# # attempts to mitigate these issues. The choices are:",
      "# #",
      "# # 1. \"none\": All output is left untouched i.e. any control chars will remain.",
      "# # 2. \"all\": All control chars and ansi sequences are stripped.",
      "# # 3. \"smart\" (default): An attempt is made to leave ansi sequences that merely",
      "# #    affect text formatting (e.g. colors, emphasis), while stripping everything",
      "# #    else. This has the potential to be the prettiest, though it is possible",
      "# #    some 'bad' sequences remain.",
      "# strip-control = \"smart\"",
      "",
      "# # String that determines how the timers is formatted. Options (and examples)",
      "# # are:",
      "# #",
      "# # digital_compact:   03:00:20",
      "# # digital_full:      00:03:00:20",
      "# # prose_compact:     3 hours, 20 seconds",
      "# # prose_full:        0 days, 3 hours, 0 minutes, 20 seconds",
      "# #",
      "# # Defaults to prose_compact",
      "# timer-format = \"prose_compact\"",
      "",
      "# #[file-log]",
      "# # Writes all logs to a file. The \"default\" option writes to the XDG state",
      "# # directory e.g. ~/.local/state/shrun/shrun.log. Any other string is",
      "# # interpreted as a PATH.",
      "# #path = \"default\"",
      "",
      "# # Like command-name-trunc, but for file logs.",
      "# #command-name-trunc = 40",
      "",
      "# # If true, the log file is deleted on a successful exit. Does not delete the",
      "# # file if shrun exited via failure.",
      "# #delete-on-success = false",
      "",
      "# # Like console-log.line-trunc, but for file logs.",
      "# # line-trunc = 150",
      "",
      "# # Mode in which to open the log file. Can be \"write\" (the default),",
      "# # \"append\", or \"rename\". The \"rename\" option will rename the requested log",
      "# # file if there is a collision e.g. '-f shrun.log' will become",
      "# # 'shrun (1).log'.",
      "# #mode = \"write\"",
      "",
      "# # Sets a threshold for the file log size, upon which we either print a",
      "# # warning or delete the file, if it is exceeded. Format is value and units",
      "# # e.g. warn 10 mb, warn 5 gigabytes, delete 20.5B. Defaults to warning at",
      "# # 50 mb. Can be disabled with \"nothing\".",
      "# #size-mode = \"warn 1 g\"",
      "",
      "# # Like command-log.strip-control, but for file logs. Defaults to all.",
      "# #strip-control = \"all\"",
      "",
      "# # If enabled, sends off notifications when commands/shrun complete.",
      "# [notify]",
      "# # Mandatory, turns on notifications.",
      "# #",
      "# # \"final\" -> sends off a notification when all commands finish",
      "# # \"command\" -> sends off a notification when each command finishes.",
      "# # \"all\" -> implies \"final\" and \"command\".",
      "# action = \"command\"",
      "",
      "# # For linux, must be one of \"dbus\" or \"notify-send\". Defaults to \"dbus\".",
      "# # OSX must be \"apple-script\" or unspecified.",
      "# system = \"notify-send\"",
      "",
      "# # Sets the timeout for non-error notifications. Defaults to 10",
      "# #",
      "# # \"never\" -> notifications never time out.",
      "# # <natural> -> non-error notifications time out in <natural> seconds.",
      "# #",
      "# # Note that this is subject to the whims of the underlying notification",
      "# # system e.g. some systems ignore timeout args (notify-send), and others",
      "# # require that error notifications never time out (FDO notification spec).",
      "# timeout = \"never\""
    ]
