{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | @since 0.3
module Functional.FuncEnv
  ( FuncEnv (..),
  )
where

import Functional.Prelude
import Shrun.Configuration.Env.Types
  ( Env,
    HasCommands (..),
    HasCompletedCmds (..),
    HasLogging (..),
    HasTimeout (..),
  )
import Shrun.Effects.Process (Process (..))
import Shrun.IO qualified as ShIO
import Shrun.Logging.RegionLogger (RegionLogger (..))
import Shrun.ShellT (ShellT)
import System.Console.Regions (ConsoleRegion)
import System.Console.Regions qualified as Regions

-- | @since 0.3
data FuncEnv = MkFuncEnv
  { coreEnv :: !Env,
    logs :: !(IORef (List Text))
  }

-- | @since 0.1
makeFieldLabelsNoPrefix ''FuncEnv

-- | @since 0.3
instance HasTimeout FuncEnv where
  getTimeout = view (#coreEnv % #timeout)

-- | @since 0.3
instance HasLogging FuncEnv where
  getCmdDisplay = view (#coreEnv % #cmdDisplay)
  getCmdLineTrunc = view (#coreEnv % #cmdLineTrunc)
  getCmdLogging = view (#coreEnv % #cmdLogging)
  getCmdNameTrunc = view (#coreEnv % #cmdNameTrunc)
  getFileLogging = view (#coreEnv % #fileLog)
  getFileLogStripControl = view (#coreEnv % #fileLogStripControl)
  getDisableLogging = view (#coreEnv % #disableLogging)
  getStripControl = view (#coreEnv % #stripControl)

-- | @since 0.3
instance HasCompletedCmds FuncEnv where
  getCompletedCmds = view (#coreEnv % #completedCmds)

-- | @since 0.3
instance HasCommands FuncEnv where
  getCommands = view (#coreEnv % #commands)

-- | @since 0.3
instance RegionLogger (ShellT FuncEnv IO) where
  type Region (ShellT FuncEnv IO) = ConsoleRegion

  logFn txt = do
    ls <- asks $ view #logs
    liftIO $ modifyIORef' ls (txt :)

  logModeToRegionFn _ _ = logFn

  withConsoleRegion = Regions.withConsoleRegion

-- | @since 0.5
instance Process (ShellT FuncEnv IO) where
  tryCmd = ShIO.tryCommand
  tryCmdStream = ShIO.tryCommandStreamNoRegion
  tryCmdStreamRegion = ShIO.tryCommandStreamRegion
