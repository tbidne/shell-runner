{-# LANGUAGE ImportQualifiedPost #-}

-- | Parses command line args into the core 'Env' type used by the main
-- application.
module ShellRun.Parsing.Env
  ( runParser,
  )
where

import Control.Applicative ((<**>), (<|>))
import Control.Applicative qualified as App
import Data.Text (Text)
import Data.Text qualified as T
import Options.Applicative (ParseError (..), Parser, ParserInfo (..))
import Options.Applicative qualified as OptApp
import Options.Applicative.Help.Chunk (Chunk (..))
import Options.Applicative.Types (ArgPolicy (..))
import ShellRun.Math (NonNegative)
import ShellRun.Math qualified as Math
import ShellRun.Types.Env (Env (..), SubLogging (..))

-- | Runs the parser.
runParser :: IO Env
runParser = OptApp.execParser parserInfo

parserInfo :: ParserInfo Env
parserInfo =
  ParserInfo
    { infoParser = envParser,
      infoFullDesc = True,
      infoProgDesc = Chunk Nothing,
      infoHeader = Chunk Nothing,
      infoFooter = Chunk Nothing,
      infoFailureCode = 1,
      infoPolicy = Intersperse
    }

envParser :: Parser Env
envParser =
  MkEnv
    <$> legendParser
    <*> timeoutParser
    <*> subLoggingParser
    <*> commandsParser
      <**> OptApp.helper

legendParser :: Parser (Maybe Text)
legendParser =
  App.optional
    ( T.pack
        <$> OptApp.strOption
          ( OptApp.long "legend"
              <> OptApp.short 'l'
              <> OptApp.help legendHelp
          )
    )
  where
    legendHelp =
      "Path to legend file, used for translating commands."
        <> " Key/value pairs have the form `key=cmd1,,cmd2,,...`"
        <> ", i.e., keys can refer to multiple commands and refer to"
        <> " other keys recursively. Lines starting with `#` are"
        <> " considered comments and ignored."

timeoutParser :: Parser (Maybe NonNegative)
timeoutParser =
  let intParser =
        OptApp.option
          readNN
          ( OptApp.long "timeout"
              <> OptApp.short 't'
              <> OptApp.help "Non-negative integer setting a timeout"
          )
   in App.optional intParser
  where
    readNN = do
      v <- OptApp.auto
      case Math.mkNonNegative v of
        Just n -> pure n
        Nothing ->
          OptApp.readerAbort $
            ErrorMsg $
              "Timeout must be non-negative, received: "
                <> show v
                <> "!"

subLoggingParser :: Parser SubLogging
subLoggingParser = noneP <|> combineP <|> nativeP
  where
    noneP =
      OptApp.flag
        None
        None
        ( OptApp.long "no-sub-logs"
            <> OptApp.help "Do not log sub-commands. This is the default"
        )
    combineP =
      OptApp.flag'
        Combine
        ( OptApp.short 'c'
            <> OptApp.long "combine-sub-logs"
            <> OptApp.help "Combine sub-commands logging with main process."
        )
    nativeP =
      OptApp.flag'
        Native
        ( OptApp.short 'n'
            <> OptApp.long "native-sub-logs"
            <> OptApp.help
              ( "Allow sub-commands to log without any interference."
                  <> " This can be useful with programs who have 'special' logging style"
                  <> ", e.g., overwriting the same line rather than newlines."
              )
        )

commandsParser :: Parser [Text]
commandsParser =
  App.some
    ( T.pack
        <$> OptApp.argument OptApp.str (OptApp.metavar "Commands...")
    )
