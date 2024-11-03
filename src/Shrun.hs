{-# LANGUAGE AllowAmbiguousTypes #-}

-- | This module is the entry point to the @Shrun@ library used by
-- the @Shrun@ executable.
module Shrun
  ( shrun,
  )
where

import DBus.Notify (UrgencyLevel (Critical, Normal))
import Data.HashSet qualified as Set
import Effectful.Concurrent.Async qualified as Async
import Effectful.Concurrent.Static (microsleep, sleep)
import Effectful.State.Static.Local
  ( State,
    evalState,
    get,
    modify,
  )
import Effectful.Time.Dynamic (TimeSpec, withTiming)
import Shrun.Configuration.Data.ConsoleLogging.TimerFormat qualified as TimerFormat
import Shrun.Configuration.Data.Core.Timeout (Timeout (MkTimeout))
import Shrun.Configuration.Data.FileLogging
  ( FileLogOpened (MkFileLogOpened),
    FileLoggingEnv,
  )
import Shrun.Configuration.Data.Notify.Action
  ( NotifyAction
      ( NotifyAll,
        NotifyCommand,
        NotifyFinal
      ),
  )
import Shrun.Configuration.Env.Types
  ( HasAnyError (getAnyError),
    HasCommandLogging,
    HasCommands (getCommands, getCompletedCommands),
    HasCommonLogging (getCommonLogging),
    HasConsoleLogging (getConsoleLogging),
    HasFileLogging (getFileLogging),
    HasInit,
    HasNotifyConfig (getNotifyConfig),
    HasTimeout (getTimeout),
    setAnyErrorTrue,
  )
import Shrun.Data.Command (CommandP1)
import Shrun.Data.Text qualified as ShrunText
import Shrun.IO
  ( CommandResult (CommandFailure, CommandSuccess),
    Stderr (MkStderr),
    tryCommandLogging,
  )
import Shrun.Logging qualified as Logging
import Shrun.Logging.Formatting qualified as LogFmt
import Shrun.Logging.RegionLogger
  ( RegionLogger,
    displayRegions,
    logGlobal,
    logRegion,
    withRegion,
  )
import Shrun.Logging.Types
  ( FileLog,
    Log (MkLog, cmd, lvl, mode, msg),
    LogLevel
      ( LevelError,
        LevelFatal,
        LevelFinished,
        LevelSuccess,
        LevelTimer,
        LevelWarn
      ),
    LogMode (LogModeFinish, LogModeSet),
    LogRegion (LogNoRegion, LogRegion),
  )
import Shrun.Notify qualified as Notify
import Shrun.Notify.Effect (Notify)
import Shrun.Prelude
import Shrun.Utils qualified as Utils

-- | Entry point
shrun ::
  forall env r es.
  ( Concurrent :> es,
    HasAnyError env,
    HasCallStack,
    HasCommands env,
    HasInit env,
    HasCommandLogging env,
    HasCommonLogging env,
    HasConsoleLogging env r,
    HasFileLogging env,
    HasNotifyConfig env,
    HasTimeout env,
    HandleReader :> es,
    HandleWriter :> es,
    IORefE :> es,
    Notify :> es,
    TypedProcess :> es,
    Reader env :> es,
    RegionLogger r :> es,
    Time :> es
  ) =>
  -- | .
  Eff es ()
shrun = displayRegions @r $ do
  mFileLogging <- asks @env getFileLogging
  (_, consoleQueue) <- asks @env (getConsoleLogging @env @r)

  -- always start console logger
  Async.withAsync (pollQueueToConsole consoleQueue) $ \consoleLogger -> do
    -- run commands, running file logger if requested
    maybe
      runCommands
      runWithFileLogging
      mFileLogging

    -- cancel consoleLogger, print remaining logs
    Async.cancel consoleLogger
    flushTBQueueA consoleQueue >>= traverse_ printConsoleLog

    -- if any processes have failed, exit with an error
    anyError <- readTVarA =<< asks @env getAnyError
    when anyError exitFailure
  where
    runWithFileLogging :: (HasCallStack) => FileLoggingEnv -> Eff es ()
    runWithFileLogging fileLogging =
      Async.withAsync (pollQueueToFile fileLogging) $ \fileLoggerThread -> do
        runCommands

        Async.cancel fileLoggerThread

        -- handle any remaining file logs
        flushTBQueueA fileQueue >>= traverse_ (logFile h)
        hFlush h
      where
        MkFileLogOpened h fileQueue = fileLogging ^. #file

    runCommands :: (HasCallStack) => Eff es ()
    runCommands = do
      cmds <- asks @env getCommands
      let actions = Async.mapConcurrently_ (runCommand @env @r) cmds
          actionsWithTimer = Async.race_ actions (counter @env @r)

      (totalTime, result) <- withTiming $ trySync actionsWithTimer
      printFinalResult @env @r totalTime result

runCommand ::
  forall env r es.
  ( HasAnyError env,
    HasCallStack,
    HasCommands env,
    HasInit env,
    HasCommandLogging env,
    HasCommonLogging env,
    HasConsoleLogging env r,
    HasFileLogging env,
    HasNotifyConfig env,
    HandleReader :> es,
    IORefE :> es,
    Notify :> es,
    TypedProcess :> es,
    Reader env :> es,
    RegionLogger r :> es,
    Concurrent :> es,
    Time :> es
  ) =>
  CommandP1 ->
  Eff es ()
runCommand cmd = do
  cmdResult <- tryCommandLogging @env @r cmd
  commonLogging <- asks @env getCommonLogging
  (consoleLogging, _) <- asks @env (getConsoleLogging @env @r)

  let timerFormat = consoleLogging ^. #timerFormat
      (urgency, msg', lvl, timeElapsed) = case cmdResult of
        -- see NOTE: [Text Line Concatentation] for how we combine the
        -- multiple texts back into a single err.
        CommandFailure t (MkStderr errs) ->
          let errMsg = ShrunText.concat errs
           in (Critical, ": " <> errMsg, LevelError, t)
        CommandSuccess t -> (Normal, "", LevelSuccess, t)
      timeMsg = TimerFormat.formatRelativeTime timerFormat timeElapsed <> msg'

  withRegion @r Linear $ \r ->
    Logging.putRegionLog @env @r r
      $ MkLog
        { cmd = Just cmd,
          msg = timeMsg,
          lvl,
          mode = LogModeFinish
        }

  let commandNameTrunc = consoleLogging ^. #commandNameTrunc
      keyHide = commonLogging ^. #keyHide
      formattedCmd = LogFmt.formatCommand keyHide commandNameTrunc cmd

  -- Sent off notif if NotifyAll or NotifyCommand is set
  cfg <- asks @env getNotifyConfig
  case cfg ^? (_Just % #action) of
    Just NotifyAll -> Notify.sendNotif @env @r (formattedCmd <> " Finished") timeMsg urgency
    Just NotifyCommand -> Notify.sendNotif @env @r (formattedCmd <> " Finished") timeMsg urgency
    _ -> pure ()

printFinalResult ::
  forall env r es e b.
  ( Exception e,
    HasAnyError env,
    HasCallStack,
    HasCommonLogging env,
    HasConsoleLogging env r,
    HasFileLogging env,
    HasNotifyConfig env,
    Notify :> es,
    Reader env :> es,
    RegionLogger r :> es,
    Concurrent :> es,
    Time :> es
  ) =>
  TimeSpec ->
  Either e b ->
  Eff es ()
printFinalResult totalTime result = withRegion @r Linear $ \r -> do
  Utils.whenLeft result $ \ex -> do
    let errMsg =
          mconcat
            [ "Encountered an exception. This is likely not an error in any ",
              "of the commands run but rather an error in Shrun itself: ",
              ShrunText.fromTextReplace $ displayExceptiont ex
            ]
        fatalLog =
          MkLog
            { cmd = Nothing,
              msg = errMsg,
              lvl = LevelFatal,
              mode = LogModeFinish
            }
    Logging.putRegionLog @env @r r fatalLog

    -- update anyError
    setAnyErrorTrue @env

  timerFormat <- asks @env (view (_1 % #timerFormat) . getConsoleLogging @_ @r)
  let totalTimeTxt =
        TimerFormat.formatRelativeTime
          timerFormat
          (Utils.timeSpecToRelTime totalTime)
      finalLog =
        MkLog
          { cmd = Nothing,
            msg = totalTimeTxt,
            lvl = LevelFinished,
            mode = LogModeFinish
          }

  -- Send off a 'finished' notification
  anyError <- readTVarA =<< asks @env getAnyError
  let urgency = if anyError then Critical else Normal

  -- Sent off notif if NotifyAll or NotifyFinal is set
  cfg <- asks @env getNotifyConfig
  case cfg ^? (_Just % #action) of
    Just NotifyAll -> Notify.sendNotif @env @r "Shrun Finished" totalTimeTxt urgency
    Just NotifyFinal -> Notify.sendNotif @env @r "Shrun Finished" totalTimeTxt urgency
    _ -> pure ()

  Logging.putRegionLog @env @r r finalLog

counter ::
  forall env r es.
  ( HasAnyError env,
    HasCallStack,
    HasCommands env,
    HasCommonLogging env,
    HasConsoleLogging env r,
    HasFileLogging env,
    HasTimeout env,
    Concurrent :> es,
    Reader env :> es,
    RegionLogger r :> es,
    Time :> es
  ) =>
  Eff es ()
counter = do
  -- HACK: This brief delay is so that our timer starts "last" i.e. after each
  -- individual command. This way the running timer console region is below all
  -- the commands' in the console.
  microsleep 100_000
  withRegion @r Linear $ \r -> do
    timeout <- asks @env getTimeout
    evalState @Natural 0
      $ Utils.whileM_ (keepRunning @env @r r timeout)
      $ do
        sleep 1
        modify @Natural (\(!x) -> x + 1)
        elapsed <- get
        logCounter @env @r r elapsed

logCounter ::
  forall env r es.
  ( HasCallStack,
    HasCommonLogging env,
    HasConsoleLogging env r,
    Reader env :> es,
    Concurrent :> es
  ) =>
  r ->
  Natural ->
  Eff es ()
logCounter region elapsed = do
  timerFormat <- asks @env (view (_1 % #timerFormat) . getConsoleLogging @_ @r)

  let msg = TimerFormat.formatSeconds timerFormat elapsed
      lg =
        MkLog
          { cmd = Nothing,
            msg,
            lvl = LevelTimer,
            mode = LogModeSet
          }
  Logging.regionLogToConsoleQueue @env region lg

keepRunning ::
  forall env r es.
  ( HasAnyError env,
    HasCallStack,
    HasCommands env,
    HasCommonLogging env,
    HasConsoleLogging env r,
    HasFileLogging env,
    Concurrent :> es,
    Reader env :> es,
    State Natural :> es,
    Time :> es
  ) =>
  r ->
  Maybe Timeout ->
  Eff es Bool
keepRunning region mto = do
  elapsed <- get
  if timedOut elapsed mto
    then do
      keyHide <- asks @env (view #keyHide . getCommonLogging)
      allCmds <- asks @env getCommands
      completedCommandsTVar <- asks @env getCompletedCommands
      completedCommands <- readTVarA completedCommandsTVar

      -- update anyError
      setAnyErrorTrue @env

      let completedCommandsSet = Set.fromList $ toList completedCommands
          allCmdsSet = Set.fromList $ toList allCmds
          incompleteCmds = Set.difference allCmdsSet completedCommandsSet
          toTxtList acc cmd = LogFmt.displayCmd cmd keyHide : acc

          unfinishedCmds =
            ShrunText.intercalate ", "
              $ foldl' toTxtList [] incompleteCmds

      Logging.putRegionLog @env @r region
        $ MkLog
          { cmd = Nothing,
            msg = "Timed out, cancelling remaining commands: " <> unfinishedCmds,
            lvl = LevelWarn,
            mode = LogModeFinish
          }
      pure False
    else pure True

timedOut :: Natural -> Maybe Timeout -> Bool
timedOut _ Nothing = False
timedOut timer (Just (MkTimeout t)) = timer > t

pollQueueToConsole ::
  forall r es void.
  ( HasCallStack,
    RegionLogger r :> es,
    Concurrent :> es
  ) =>
  TBQueue (LogRegion r) ->
  Eff es void
pollQueueToConsole queue = do
  -- NOTE: Same masking behavior as pollQueueToFile.
  forever $ atomicReadWrite queue printConsoleLog

printConsoleLog ::
  forall r es.
  ( HasCallStack,
    RegionLogger r :> es
  ) =>
  LogRegion r ->
  Eff es ()
printConsoleLog (LogNoRegion consoleLog) = logGlobal @r (consoleLog ^. #unConsoleLog)
printConsoleLog (LogRegion m r consoleLog) = logRegion m r (consoleLog ^. #unConsoleLog)

pollQueueToFile ::
  forall es void.
  ( HasCallStack,
    HandleWriter :> es,
    Concurrent :> es
  ) =>
  FileLoggingEnv ->
  Eff es void
pollQueueToFile fileLogging = do
  forever
    $
    -- NOTE: Read+write needs to be atomic, otherwise we can lose logs
    -- (i.e. thread reads the log and is cancelled before it can write it).
    -- Hence the mask.
    atomicReadWrite queue (logFile @es h)
  where
    MkFileLogOpened h queue = fileLogging ^. #file

logFile :: (HasCallStack, HandleWriter :> es) => Handle -> FileLog -> Eff es ()
logFile h = (\t -> hPutUtf8 h t *> hFlush h) . view #unFileLog

-- | Reads from a queue and applies the function, if we receive a value.
-- Atomic in the sense that if a read is successful, then we will apply the
-- given function, even if an async exception is raised.
atomicReadWrite ::
  ( HasCallStack,
    Concurrent :> es
  ) =>
  -- | Queue from which to read.
  TBQueue a ->
  -- | Function to apply.
  (a -> Eff es b) ->
  Eff es ()
atomicReadWrite queue logAction =
  mask $ \restore -> restore (readTBQueueA queue) >>= void . logAction
