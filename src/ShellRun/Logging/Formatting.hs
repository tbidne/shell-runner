-- | Provides 'Log' formatting functionality.
--
-- @since 0.1
module ShellRun.Logging.Formatting
  ( formatConsoleLog,
    displayCmd,
    displayCmd',
  )
where

import ShellRun.Command (Command (..))
import ShellRun.Data.InfNum (PosInfNum (..))
import ShellRun.Env.Types
  ( CmdDisplay (..),
    HasCmdDisplay (..),
    HasCmdLineTrunc (..),
    HasCmdNameTrunc (..),
    Truncation (..),
  )
import ShellRun.Logging.Log (Log (..), LogLevel (..))
import ShellRun.Logging.Log qualified as Log
import ShellRun.Prelude
import ShellRun.Utils qualified as U
import System.Console.Pretty qualified as P

-- | Formats a log to be printed to the console.
--
-- @since 0.1
formatConsoleLog ::
  ( HasCmdDisplay env,
    HasCmdNameTrunc env,
    HasCmdLineTrunc env,
    MonadReader env m
  ) =>
  Log ->
  m Text
formatConsoleLog log@MkLog {cmd, msg, lvl} = do
  MkTruncation cmdNameTrunc <- asks getCmdNameTrunc
  MkTruncation lineNameTrunc <- asks getCmdLineTrunc
  case cmd of
    Nothing -> pure $ colorize $ prefix <> msg
    Just com -> do
      -- get cmd name to display
      name <- displayCmd com
      let -- truncate cmd/name if necessary
          name' = case cmdNameTrunc of
            PPosInf -> name
            PFin n -> U.truncateIfNeeded n name
          -- truncate entire if necessary (flag on and command log only)
          line = colorize $ prefix <> "[" <> name' <> "] " <> msg
          line' = case (lvl, lineNameTrunc) of
            (SubCommand, PFin m) -> U.truncateIfNeeded m line
            _ -> line
       in pure line'
  where
    colorize = P.color $ Log.logToColor log
    prefix = Log.logToPrefix log
{-# INLINEABLE formatConsoleLog #-}

-- | Variant of 'displayCmd\'' using 'MonadReader'.
--
-- @since 0.1
displayCmd :: (HasCmdDisplay env, MonadReader env m) => Command -> m Text
displayCmd cmd = asks getCmdDisplay <&> displayCmd' cmd
{-# INLINEABLE displayCmd #-}

-- | Pretty show for 'Command'. If the command has a key, and 'CmdDisplay' is
-- 'ShowKey' then we return the key. Otherwise we return the command itself.
--
-- >>> displayCmd' (MkCommand Nothing "some long command") ShowCmd
-- "some long command"
--
-- >>> displayCmd' (MkCommand Nothing "some long command") ShowKey
-- "some long command"
--
-- >>> displayCmd' (MkCommand (Just "long") "some long command") ShowCmd
-- "some long command"
--
-- >>> displayCmd' (MkCommand (Just "long") "some long command") ShowKey
-- "long"
--
-- @since 0.1
displayCmd' :: Command -> CmdDisplay -> Text
displayCmd' (MkCommand (Just key) _) ShowKey = key
displayCmd' (MkCommand _ cmd) _ = cmd
{-# INLINEABLE displayCmd' #-}
