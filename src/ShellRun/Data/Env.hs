-- | Provides core 'Env' types.
--
-- @since 0.1.0.0
module ShellRun.Data.Env
  ( -- * \"HasX\" style typeclasses required for our concrete Env type.
    HasCommandDisplay (..),
    HasCommandLogging (..),
    HasFileLogging (..),
    HasTimeout (..),

    -- * Types
    Timeout,
    Env (..),
    CommandDisplay (..),
    CommandLogging (..),
  )
where

import ShellRun.Data.Supremum (Supremum (..))
import ShellRun.Data.Timeout (Timeout)
import ShellRun.Env (HasCommands (..), HasLegend (..))
import ShellRun.Logging.Queue (LogTextQueue)
import ShellRun.Prelude

-- | The main 'Env' type used by ShellRun. Intended to be used with
-- 'ShellRun.Class.MonadReader'.
--
-- @since 0.1.0.0
data Env = MkEnv
  { -- | Optional path to a legend file.
    --
    -- @since 0.1.0.0
    legend :: Maybe FilePath,
    -- | Optional timeout.
    --
    -- @since 0.1.0.0
    timeout :: Maybe Timeout,
    -- | Optional file-logging. If enabled, holds the path toe fhe file
    -- and the log queue.
    --
    -- @since 0.1.0.0
    fileLogging :: Maybe (Tuple2 FilePath LogTextQueue),
    -- | Whether to log commands.
    --
    -- @since 0.1.0.0
    commandLogging :: CommandLogging,
    -- | Whether to display the command (key) names or the commands
    -- themselves.
    --
    -- @since 0.1.0.0
    commandDisplay :: CommandDisplay,
    -- | The commands to run.
    --
    -- @since 0.1.0.0
    commands :: List Text
  }
  deriving
    ( -- | @since 0.1.0.0
      Show
    )

-- | Type for determining if we stream commands' logs.
--
-- @since 0.1.0.0
data CommandLogging
  = -- | No logging of sub-commands.
    --
    -- @since 0.1.0.0
    Disabled
  | -- | Logging of sub-commands
    --
    -- @since 0.1.0.0
    Enabled
  deriving stock
    ( -- | @since 0.1.0.0
      Bounded,
      -- | @since 0.1.0.0
      Eq,
      -- | @since 0.1.0.0
      Ord,
      -- | @since 0.1.0.0
      Show
    )
  deriving
    ( -- | @since 0.1.0.0
      Semigroup,
      -- | @since 0.1.0.0
      Monoid
    )
    via Supremum CommandLogging

-- | Type for determining if we use the command's key
-- for display, rather than the key itself.
data CommandDisplay
  = -- | Display the command itself, not the key.
    -- @since 0.1.0.0
    ShowCommand
  | -- | Display the command's key, if it exists, rather
    -- than the key itself.
    --
    -- @since 0.1.0.0
    ShowKey
  deriving stock
    ( -- | @since 0.1.0.0
      Bounded,
      -- | @since 0.1.0.0
      Eq,
      -- | @since 0.1.0.0
      Ord,
      -- | @since 0.1.0.0
      Show
    )
  deriving
    ( -- | @since 0.1.0.0
      Semigroup,
      -- | @since 0.1.0.0
      Monoid
    )
    via Supremum CommandDisplay

-- | Timeout, if any.
--
-- @since 0.1.0.0
class HasTimeout env where
  -- | @since 0.1.0.0
  getTimeout :: env -> Maybe Timeout

-- | FileLogging, if any.
--
-- @since 0.1.0.0
class HasFileLogging env where
  -- | @since 0.1.0.0
  getFileLogging :: env -> Maybe (FilePath, LogTextQueue)

-- | Determines if we should log commands' output.
--
-- @since 0.1.0.0
class HasCommandLogging env where
  getCommandLogging :: env -> CommandLogging

-- | Determines how to display command names.
--
-- @since 0.1.0.0
class HasCommandDisplay env where
  -- | @since 0.1.0.0
  getCommandDisplay :: env -> CommandDisplay

-- | @since 0.1.0.0
instance HasLegend Env where
  getLegend :: Env -> Maybe FilePath
  getLegend = legend

-- | @since 0.1.0.0
instance HasTimeout Env where
  getTimeout :: Env -> Maybe Timeout
  getTimeout = timeout

-- | @since 0.1.0.0
instance HasFileLogging Env where
  getFileLogging :: Env -> Maybe (FilePath, LogTextQueue)
  getFileLogging = fileLogging

-- | @since 0.1.0.0
instance HasCommandLogging Env where
  getCommandLogging :: Env -> CommandLogging
  getCommandLogging = commandLogging

-- | @since 0.1.0.0
instance HasCommandDisplay Env where
  getCommandDisplay :: Env -> CommandDisplay
  getCommandDisplay = commandDisplay

-- | @since 0.1.0.0
instance HasCommands Env where
  getCommands :: Env -> List Text
  getCommands = commands
