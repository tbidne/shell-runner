-- | Provides type for notifications.
module Shrun.Configuration.Data.Notify.Action
  ( NotifyAction (..),
    parseNotifyAction,
    notifyActionStr,
  )
where

import Shrun.Prelude
import Shrun.Utils qualified as Utils

-- | Determines for which actions we should send notifications.
data NotifyAction
  = -- | Send a notification after all commands are completed.
    NotifyFinal
  | -- | Send notifications when each command completes.
    NotifyCommand
  | -- | NotifyFinal and NotifyCommand.
    NotifyAll
  deriving stock (Eq, Show)

instance DecodeTOML NotifyAction where
  tomlDecoder = parseNotifyAction tomlDecoder

-- | Parses 'NotifyAction'.
parseNotifyAction :: (MonadFail m) => m Text -> m NotifyAction
parseNotifyAction getTxt =
  getTxt >>= \case
    "final" -> pure NotifyFinal
    "command" -> pure NotifyCommand
    "all" -> pure NotifyAll
    bad ->
      fail
        $ Utils.fmtUnrecognizedError
          "notify action"
          notifyActionStr
          (unpack bad)

-- | Available 'NotifyAction' strings.
notifyActionStr :: (IsString a) => a
notifyActionStr = "(final | command | all)"
