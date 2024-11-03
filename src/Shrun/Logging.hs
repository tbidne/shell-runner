{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Provides logging functionality. This is a high-level picture of how
-- logging works:
--
-- 1. "Shrun.IO" sends logs per command based on the environment (i.e. is file
--    logging on and/or do we log commands). If any logs are produced, they
--    are formatted and sent directly to a queue.
--
-- 2. "Shrun" also produces logs. These are "higher-level" e.g. success/failure
--    status of a given command, fatal errors, etc. "Shrun" uses the functions
--    here (e.g. putRegionLog) that handles deciding if a given log
--    should be written to either/both of the console/file log queues.
--
-- 3. "Shrun" has two threads -- one for each queue -- that poll their
--    respective queues and writes logs as they are found. These do no
--    environment checking; any logs that make it to the queue are eventually
--    written.
module Shrun.Logging
  ( -- * Writing logs
    putRegionLog,
    regionLogToConsoleQueue,
    logToFileQueue,
  )
where

import Shrun.Configuration.Data.CommonLogging.KeyHideSwitch (KeyHideSwitch)
import Shrun.Configuration.Data.FileLogging (FileLoggingEnv)
import Shrun.Configuration.Env.Types
  ( HasCommonLogging (getCommonLogging),
    HasConsoleLogging (getConsoleLogging),
    HasFileLogging (getFileLogging),
  )
import Shrun.Logging.Formatting (formatConsoleLog, formatFileLog)
import Shrun.Logging.Types
  ( Log,
    LogRegion (LogRegion),
  )
import Shrun.Prelude

-- | Unconditionally writes a log to the console queue. Conditionally
-- writes the log to the file queue, if 'Logging'\'s @fileLogging@ is
-- present.
putRegionLog ::
  forall env r es.
  ( HasCallStack,
    HasCommonLogging env,
    HasConsoleLogging env r,
    HasFileLogging env,
    Reader env :> es,
    Concurrent :> es,
    Time :> es
  ) =>
  -- | Region.
  r ->
  -- | Log to send.
  Log ->
  Eff es ()
putRegionLog region lg = do
  commonLogging <- asks @env getCommonLogging
  mFileLogging <- asks @env getFileLogging

  let keyHide = commonLogging ^. #keyHide

  regionLogToConsoleQueue @env region lg
  for_ mFileLogging (\fl -> logToFileQueue keyHide fl lg)

-- | Writes the log to the console queue.
regionLogToConsoleQueue ::
  forall env es r.
  ( HasCallStack,
    HasCommonLogging env,
    HasConsoleLogging env r,
    Reader env :> es,
    Concurrent :> es
  ) =>
  -- | Region.
  r ->
  -- | Log to send.
  Log ->
  Eff es ()
regionLogToConsoleQueue region log = do
  keyHide <- asks @env (view #keyHide . getCommonLogging)
  (consoleLogging, queue) <- asks @env getConsoleLogging

  let formatted = formatConsoleLog keyHide consoleLogging log

  writeTBQueueA queue (LogRegion (log ^. #mode) region formatted)

-- | Writes the log to the file queue.
logToFileQueue ::
  ( HasCallStack,
    Concurrent :> es,
    Time :> es
  ) =>
  -- | How to display the command.
  KeyHideSwitch ->
  -- | FileLogging config.
  FileLoggingEnv ->
  -- | Log to send.
  Log ->
  Eff es ()
logToFileQueue keyHide fileLogging log = do
  formatted <- formatFileLog keyHide fileLogging log
  writeTBQueueA (fileLogging ^. #file % #queue) formatted
