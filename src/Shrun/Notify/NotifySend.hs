-- | Effect for NotifySend.
module Shrun.Notify.NotifySend
  ( notifyNotifySend,
  )
where

import DBus.Notify (UrgencyLevel (Critical, Low, Normal))
import Data.Text qualified as T
import Effectful.Process.Typed qualified as P
import Shrun.Configuration.Data.Notify.System (NotifySystemP (NotifySend))
import Shrun.Configuration.Data.Notify.Timeout
  ( NotifyTimeout
      ( NotifyTimeoutNever,
        NotifyTimeoutSeconds
      ),
  )
import Shrun.Notify.Effect
  ( NotifyException (MkNotifyException),
    ShrunNote,
    exitFailureToStderr,
  )
import Shrun.Prelude
import Shrun.Utils qualified as Utils

-- FIXME: Test the lack of HasCallStack here.
-- Also notifyNotifySend should probably throw for OSX.

notifyNotifySend ::
  ( TypedProcess :> es
  ) =>
  ShrunNote ->
  Eff es (Maybe NotifyException)
notifyNotifySend note =
  notify (shrunToNotifySend note) <<&>> \stderr ->
    MkNotifyException note NotifySend (decodeUtf8Lenient stderr)
  where
    notify =
      fmap exitFailureToStderr
        . P.readProcessStderr
        . P.shell
        . T.unpack

shrunToNotifySend :: ShrunNote -> Text
shrunToNotifySend shrunNote = txt
  where
    txt =
      mconcat
        [ "notify-send ",
          " --app-name Shrun \"",
          summary,
          "\" ",
          (\b -> " \"" <> b <> "\" ") body,
          ulToNS (shrunNote ^. #urgency),
          timeout
        ]

    -- Encountered a bug where notify-send would error when given commands
    -- from the legend that contained quotes and --common-log-key-hide was active.
    -- This is presumably due to the command in the logs like
    --
    --     [Command][some cmd "with quotes"]...
    --
    -- which was then not properly escaped when sent off to notify-send.
    -- Technically the reproducer:
    --
    --     shrun --common-log-key-hide --notify-system notify-send --config=examples/config.toml ui
    --
    -- only required escaping the summary, but we do the same to the body out
    -- of paranoia.
    summary = Utils.escapeDoubleQuotes $ shrunNote ^. #summary % #unUnlinedText
    body = Utils.escapeDoubleQuotes $ shrunNote ^. #body % #unUnlinedText

    ulToNS Low = " --urgency low "
    ulToNS Normal = " --urgency normal "
    ulToNS Critical = " --urgency critical "

    timeout = case shrunNote ^. #timeout of
      NotifyTimeoutNever -> " --expire-time 0 "
      NotifyTimeoutSeconds s ->
        mconcat
          [ " --expire-time ",
            showt (unsafeConvertIntegral @_ @Integer s * 1_000)
          ]
