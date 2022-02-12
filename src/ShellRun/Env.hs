-- | Module defining \"HasX\" style typeclasses for the environment
-- needed by ShellRun.
module ShellRun.Env
  ( HasCommands (..),
    HasLegend (..),
  )
where

import ShellRun.Prelude

-- | Path to legend file.
class HasLegend env where
  getLegend :: env -> Maybe FilePath

-- | The commands themselves.
class HasCommands env where
  getCommands :: env -> List Text
