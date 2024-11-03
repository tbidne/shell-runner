{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Provides the low-level `IO` functions for running shell commands.
module Shrun.IO
  ( -- * Types
    CommandResult (..),
    Stderr (..),

    -- * Running commands
    tryCommandLogging,
  )
where

import Data.ByteString.Lazy qualified as BSL
import Data.Time.Relative (RelativeTime)
import Effectful.Concurrent.Static (microsleep)
import Effectful.Process.Typed (ProcessConfig)
import Effectful.Process.Typed qualified as P
import Effectful.Time.Dynamic (getMonotonicTime, withTiming)
import Shrun.Configuration.Data.CommandLogging
  ( BufferLength,
    BufferTimeout,
    ReportReadErrorsSwitch
      ( ReportReadErrorsOff,
        ReportReadErrorsOn
      ),
  )
import Shrun.Configuration.Data.CommandLogging.ReadStrategy
  ( ReadStrategy
      ( ReadBlock,
        ReadBlockLineBuffer
      ),
  )
import Shrun.Configuration.Data.ConsoleLogging
  ( ConsoleLogCmdSwitch
      ( ConsoleLogCmdOff,
        ConsoleLogCmdOn
      ),
  )
import Shrun.Configuration.Env.Types
  ( HasAnyError,
    HasCommandLogging (getCommandLogging),
    HasCommands,
    HasCommonLogging (getCommonLogging),
    HasConsoleLogging (getConsoleLogging),
    HasFileLogging (getFileLogging),
    HasInit (getInit),
    prependCompletedCommand,
    setAnyErrorTrue,
  )
import Shrun.Data.Command (CommandP1, commandToProcess)
import Shrun.Data.Text (UnlinedText)
import Shrun.Data.Text qualified as ShrunText
import Shrun.IO.Handle
  ( ReadHandleResult (ReadErr, ReadErrSuccess, ReadNoData, ReadSuccess),
  )
import Shrun.IO.Handle qualified as Handle
import Shrun.Logging.Formatting (formatConsoleLog, formatFileLog)
import Shrun.Logging.RegionLogger (RegionLogger, withRegion)
import Shrun.Logging.Types
  ( Log (MkLog, cmd, lvl, mode, msg),
    LogLevel (LevelCommand),
    LogMode (LogModeSet),
    LogRegion (LogRegion),
  )
import Shrun.Prelude
import Shrun.Utils qualified as U

-- | Newtype wrapper for stderr.
newtype Stderr = MkStderr {unStderr :: List UnlinedText}
  deriving stock (Eq, Show)

-- | Turns a 'ReadHandleResult' into a 'Stderr'.
readHandleResultToStderr :: ReadHandleResult -> Stderr
readHandleResultToStderr ReadNoData = MkStderr $ ShrunText.fromText "<No data>"
readHandleResultToStderr (ReadErr errs) = MkStderr (neToList errs)
readHandleResultToStderr (ReadSuccess errs) = MkStderr (neToList errs)
readHandleResultToStderr (ReadErrSuccess e1 e2) = MkStderr (neToList $ e1 <> e2)

-- | Result of running a command.
data CommandResult
  = CommandSuccess RelativeTime
  | CommandFailure RelativeTime Stderr
  deriving stock (Eq, Show)

-- | Runs the command, returns ('ExitCode', 'Stderr')
shExitCode ::
  forall env es.
  ( HasCallStack,
    HasInit env,
    Reader env :> es,
    TypedProcess :> es
  ) =>
  CommandP1 ->
  Eff es (ExitCode, Stderr)
shExitCode cmd = do
  process <- commandToProcess cmd <$> asks @env getInit
  (exitCode, _stdout, stderr) <- P.readProcess process
  pure (exitCode, wrap (MkStderr . ShrunText.fromText) stderr)
  where
    wrap f = f . decodeUtf8Lenient . BSL.toStrict

-- | Version of 'shExitCode' that returns 'Left' 'Stderr' if there is a failure,
-- 'Right' 'Stdout' otherwise.
tryShExitCode ::
  forall env es.
  ( HasCallStack,
    HasInit env,
    Reader env :> es,
    TypedProcess :> es
  ) =>
  CommandP1 ->
  Eff es (Maybe Stderr)
tryShExitCode cmd =
  shExitCode @env cmd <&> \case
    (ExitSuccess, _) -> Nothing
    (ExitFailure _, stderr) -> Just stderr

-- | Runs the command, returning the time elapsed along with a possible
-- error.
tryCommandLogging ::
  forall env r es.
  ( HasAnyError env,
    HasCallStack,
    HasCommands env,
    HasInit env,
    HasCommandLogging env,
    HasCommonLogging env,
    HasConsoleLogging env r,
    HasFileLogging env,
    HandleReader :> es,
    IORefE :> es,
    Reader env :> es,
    RegionLogger r :> es,
    Concurrent :> es,
    Time :> es,
    TypedProcess :> es
  ) =>
  -- | Command to run.
  CommandP1 ->
  -- | Result.
  Eff es CommandResult
tryCommandLogging command = do
  -- NOTE: We do not want tryCommandLogging to throw sync exceptions, as that
  -- will take down the whole app. tryCommandStream and tryShExitCode should be
  -- total, but there are still a few functions here that can throw. To wit:
  --
  -- - atomically: Used in prependCompletedCommand, setAnyErrorTrue,
  --               writeTBQueueA.
  -- - getSystemTimeString: Used in formatFileLog.
  --
  -- We could catch these exceptions and simply print an error. However, both
  -- of these errors have nothing to do with the actual command that is being
  -- run and point to something wrong with shrun itself. Morever, "recovery"
  -- in these instances is unclear, as we are either dropping logs (how do we
  -- report these errors?) or failing to get the time (how should we log?).
  --
  -- Thus the most reasonable course of action is to let shrun die and print
  -- the actual error so it can be fixed.

  commonLogging <- asks @env getCommonLogging
  (consoleLogging, consoleLogQueue) <- asks @env getConsoleLogging
  mFileLogging <- asks @env getFileLogging
  let keyHide = commonLogging ^. #keyHide

  let cmdFn = case (consoleLogging ^. #commandLogging, mFileLogging) of
        -- 1. No CommandLogging and no FileLogging: No streaming at all.
        (ConsoleLogCmdOff, Nothing) -> tryShExitCode @env
        -- 3. CommandLogging but no FileLogging. Stream.
        (ConsoleLogCmdOn, Nothing) -> \cmd ->
          withRegion @r Linear $ \region -> do
            let logFn = logConsole keyHide consoleLogQueue region consoleLogging

            logFn hello

            tryCommandStream @env logFn cmd
        -- 3. No CommandLogging but FileLogging: Stream (to file) but no console
        --    region.
        (ConsoleLogCmdOff, Just fileLogging) -> \cmd -> do
          let logFn :: Log -> Eff es ()
              logFn = logFile keyHide fileLogging

          logFn hello

          tryCommandStream @env logFn cmd
        -- 4. CommandLogging and FileLogging: Stream (to both) and create console
        --    region.
        (ConsoleLogCmdOn, Just fileLogging) -> \cmd ->
          withRegion Linear $ \region -> do
            let logFn log = do
                  logConsole keyHide consoleLogQueue region consoleLogging log
                  logFile keyHide fileLogging log

            logFn hello

            tryCommandStream @env logFn cmd

  withTiming (cmdFn command) >>= \case
    (rt, Nothing) -> do
      -- update completed commands
      prependCompletedCommand @env command

      pure $ CommandSuccess $ U.timeSpecToRelTime rt
    (rt, Just err) -> do
      -- update completed commands
      prependCompletedCommand @env command

      -- update anyError
      setAnyErrorTrue @env

      pure $ CommandFailure (U.timeSpecToRelTime rt) err
  where
    logConsole keyHide consoleQueue region consoleLogging log = do
      let formatted = formatConsoleLog keyHide consoleLogging log
      writeTBQueueA consoleQueue (LogRegion (log ^. #mode) region formatted)

    logFile keyHide fileLogging log = do
      formatted <- formatFileLog keyHide fileLogging log
      writeTBQueueA (fileLogging ^. #file % #queue) formatted

    hello =
      MkLog
        { cmd = Just command,
          msg = "Starting...",
          lvl = LevelCommand,
          mode = LogModeSet
        }

-- | Similar to 'tryCommand' except we attempt to stream the commands' output
-- instead of the usual swallowing.
tryCommandStream ::
  forall env es.
  ( Concurrent :> es,
    HasInit env,
    HasCallStack,
    HasCommandLogging env,
    HandleReader :> es,
    IORefE :> es,
    Reader env :> es,
    Time :> es,
    TypedProcess :> es
  ) =>
  -- | Function to apply to streamed logs.
  (Log -> Eff es ()) ->
  -- | Command to run.
  CommandP1 ->
  -- | Error, if any. Note that this will be 'Just' iff the command exited
  -- with an error, even if the error message itself is blank.
  Eff es (Maybe Stderr)
tryCommandStream logFn cmd = do
  let initToConfig :: Maybe Text -> ProcessConfig () Handle Handle
      initToConfig =
        P.setStderr P.createPipe
          . P.setStdout P.createPipe
          . commandToProcess cmd

  procConfig <- initToConfig <$> asks @env getInit

  (exitCode, finalData) <-
    P.withProcessWait procConfig (streamOutput @env logFn cmd)

  pure $ case exitCode of
    ExitSuccess -> Nothing
    ExitFailure _ -> Just $ readHandleResultToStderr finalData

-- [Blocking / Streaming output]. Leaving this here as documentation.
--
--  where
--    -- copy of P.createPipe except we set the buffering
--    createPipe' = P.mkPipeStreamSpec $ \_ h -> do
--      hSetBuffering h NoBuffering
--      pure (h, hClose h)

streamOutput ::
  forall env es.
  ( Concurrent :> es,
    HasCallStack,
    HasCommandLogging env,
    HandleReader :> es,
    IORefE :> es,
    Reader env :> es,
    Time :> es,
    TypedProcess :> es
  ) =>
  -- | Function to apply to streamed logs.
  (Log -> Eff es ()) ->
  -- | Command that was run.
  CommandP1 ->
  -- | Process handle.
  Process () Handle Handle ->
  -- | Exit code along w/ any leftover data.
  Eff es (Tuple2 ExitCode ReadHandleResult)
streamOutput logFn cmd p = do
  -- NOTE: [Saving final error message]
  --
  -- We want to save the final error message if it exists, so that we can
  -- report it to the user. Programs can be inconsistent where they report
  -- errors, so we read both stdout and stderr, prioritizing the latter when
  -- both exist.
  commandLogging <- asks @env getCommandLogging

  let bufferLength = commandLogging ^. #bufferLength
      bufferTimeout = commandLogging ^. #bufferTimeout
      reportReadErrors = commandLogging ^. #reportReadErrors

      pollInterval :: Natural
      pollInterval = commandLogging ^. (#pollInterval % #unPollInterval)

      sleepFn :: Eff es ()
      sleepFn = when (pollInterval /= 0) (microsleep pollInterval)

      blockSize :: Int
      blockSize = commandLogging ^. (#readSize % #unReadSize % _MkBytes)

      readStrategy = commandLogging ^. #readStrategy

      handleToParams ::
        Handle ->
        Eff
          es
          ( Tuple3
              (IORef ReadHandleResult)
              (IORef (Maybe UnlinedText))
              (Eff es ReadHandleResult)
          )
      handleToParams =
        mkHandleParams
          blockSize
          readStrategy
          bufferLength
          bufferTimeout

      outHandle = P.getStdout p
      errHandle = P.getStderr p

  -- - lastReadXRef: The result of the last read for handle X.
  --
  -- - prevReadXRef: Whatever was leftover from the last read for handle X.
  --   This is part of the "read block line buffer" strategy i.e. only
  --   contains "partial data" from the previous read when it exists
  --   and we are using that strategy. Compare to lastReadXRef, which
  --   __always__ contains the results for the last read, for the purposes
  --   of error reporting.
  --
  -- - readBlockX: Function for reading from handle X.

  (lastReadOutRef, prevReadOutRef, readBlockOut) <- handleToParams outHandle
  (lastReadErrRef, prevReadErrRef, readBlockErr) <- handleToParams errHandle

  exitCode <- U.untilJust $ do
    -- We need to read from both stdout and stderr -- regardless of if we
    -- created a single pipe in tryCommandStream -- or else we will miss
    -- messages
    outResult <- readBlockOut
    errResult <- readBlockErr

    writeLog logFn reportReadErrors cmd lastReadOutRef outResult
    writeLog logFn reportReadErrors cmd lastReadErrRef errResult

    -- NOTE: IF we do not have a sleep here then the CPU blows up. Adding
    -- a delay helps keep the CPU reasonable.
    sleepFn

    P.getExitCode p

  -- These are the final reads while the process was running.
  lastReadOut <- readIORef lastReadOutRef
  lastReadErr <- readIORef lastReadErrRef

  -- Leftover data. We need this as the process can exit before everything
  -- is read.
  (remainingOut, remainingErr) <- do
    -- This branch is really a paranoid "ensure we didn't change anything" if
    -- using the ReadBlock strategy. It is possible ReadBlockLineBuffer behaves
    -- the same most of the time; indeed, all of the tests pass with the
    -- normal ReadBlock strategy above even if we use ReadBlockLineBuffer
    -- below.
    case commandLogging ^. #readStrategy of
      ReadBlock -> (,) <$> readBlockOut <*> readBlockErr
      ReadBlockLineBuffer -> do
        (,)
          <$> readFinalWithPrev blockSize outHandle prevReadOutRef
          <*> readFinalWithPrev blockSize errHandle prevReadErrRef

  writeLog logFn reportReadErrors cmd lastReadOutRef remainingOut
  writeLog logFn reportReadErrors cmd lastReadErrRef remainingErr

  -- NOTE: [Stderr reporting]
  --
  -- In the event of a process failure (exitCode == ExitFailure), we want to
  -- return the last (successful) read, as it is the most likely to have
  -- relevant information. We have two possible reads here:
  --
  -- 1. The last read while the process was running (lastReadErr)
  -- 2. A final read after the process exited (remainingErr)
  --
  -- We prioritize (Semigroup), in order:
  --
  -- 1. remainingErr
  -- 2. lastReadErr
  -- 3. remainingOut
  -- 4. lastReadOut
  --
  -- We make the assumption that the most recent Stderr is the most likely to
  -- have the relevant error message, though we fall back to stdout as this
  -- is not always true.
  let finalData =
        mconcat
          [ remainingErr,
            lastReadErr,
            remainingOut,
            lastReadOut
          ]

  pure (exitCode, finalData)

-- | Create params for reading from the handle.
mkHandleParams ::
  ( HasCallStack,
    HandleReader :> es,
    IORefE :> es,
    Time :> es
  ) =>
  -- | Read block size.
  Int ->
  -- | Read strategy.
  ReadStrategy ->
  -- | Max buffer length, for read-block-line-buffer strategy.
  BufferLength ->
  -- | Max buffer time, for read-block-line-buffer strategy.
  BufferTimeout ->
  -- | Handle from which to read.
  Handle ->
  -- | Returns:
  --
  --  1. Ref for the last read (always active).
  --  2. Ref for previous partial read (only for read-block-line-buffer
  --     strategy).
  --  3. Read function.
  Eff
    es
    ( Tuple3
        (IORef ReadHandleResult)
        (IORef (Maybe UnlinedText))
        (Eff es ReadHandleResult)
    )
mkHandleParams blockSize readStrategy bufLength bufTimeout handle = do
  lastReadRef <- newIORef ReadNoData
  prevReadRef <- newIORef Nothing

  currTime <- getMonotonicTime
  bufFlushTimeRef <- newIORef currTime

  let readFn = case readStrategy of
        ReadBlock -> Handle.readHandle Nothing blockSize handle
        ReadBlockLineBuffer ->
          let outBufferParams = (prevReadRef, bufLength, bufTimeout, bufFlushTimeRef)
           in Handle.readHandle (Just outBufferParams) blockSize handle

  pure (lastReadRef, prevReadRef, readFn)

-- | Final read after the process has exited, to retrieve leftover data.
-- Only used with the read-block-line-buffer strategy.
readFinalWithPrev ::
  ( HasCallStack,
    HandleReader :> es,
    IORefE :> es
  ) =>
  -- | Block size.
  Int ->
  -- | Handle from which to read.
  Handle ->
  -- | Previous partial read.
  IORef (Maybe UnlinedText) ->
  -- | Result.
  Eff es ReadHandleResult
readFinalWithPrev blockSize handle prevReadRef = do
  Handle.readHandleRaw blockSize handle >>= \case
    -- Do not care about errors here, since we may still have leftover
    -- data that we need to get. If we cared, we could log the errors
    -- here, but it seems minor.
    Left _ -> Handle.readAndUpdateRefFinal prevReadRef ""
    Right bs -> Handle.readAndUpdateRefFinal prevReadRef bs

-- We occasionally get invalid reads here -- usually when the command
-- exits -- likely due to a race condition. It would be nice to
-- prevent these entirely, but for now ignore them, as it does not
-- appear that we ever lose important messages.
--
-- EDIT: Possibly fixed by switch to typed-process and
-- https://github.com/fpco/typed-process/issues/25?
--
-- See Note [EOF / blocking error]
writeLog ::
  ( IORefE :> es
  ) =>
  (Log -> Eff es ()) ->
  ReportReadErrorsSwitch ->
  CommandP1 ->
  IORef ReadHandleResult ->
  ReadHandleResult ->
  Eff es ()
writeLog _ _ _ _ ReadNoData = pure ()
writeLog _ ReportReadErrorsOff _ _ (ReadErr _) = pure ()
writeLog logFn ReportReadErrorsOn cmd lastReadRef r@(ReadErr messages) =
  writeLogHelper logFn cmd lastReadRef r messages
writeLog logFn reportReadErrors cmd lastReadRef r@(ReadErrSuccess errs successes) = do
  when (reportReadErrors == ReportReadErrorsOn) $ writeLogHelper logFn cmd lastReadRef r errs
  writeLogHelper logFn cmd lastReadRef r successes
writeLog logFn _ cmd lastReadRef r@(ReadSuccess messages) =
  writeLogHelper logFn cmd lastReadRef r messages

writeLogHelper ::
  ( IORefE :> es
  ) =>
  (Log -> Eff es b) ->
  CommandP1 ->
  IORef ReadHandleResult ->
  ReadHandleResult ->
  NonEmpty UnlinedText ->
  Eff es ()
writeLogHelper logFn cmd lastReadRef handleResult messages = do
  writeIORef lastReadRef handleResult
  for_ messages $ \msg ->
    logFn
      $ MkLog
        { cmd = Just cmd,
          msg,
          lvl = LevelCommand,
          mode = LogModeSet
        }
