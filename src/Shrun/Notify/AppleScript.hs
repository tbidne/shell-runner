-- | Effect for AppleScript.
module Shrun.Notify.AppleScript
  ( notifyAppleScript,
  )
where

import Data.Text qualified as T
import Effectful.Process.Typed qualified as P
import Shrun.Configuration.Data.Notify.System (NotifySystemP (AppleScript))
import Shrun.Notify.Effect
  ( NotifyException (MkNotifyException),
    ShrunNote,
    exitFailureToStderr,
  )
import Shrun.Prelude

-- FIXME: notifyAppleScript should probably throw for linux.

notifyAppleScript ::
  ( TypedProcess :> es
  ) =>
  ShrunNote ->
  Eff es (Maybe NotifyException)
notifyAppleScript note =
  notify (shrunToAppleScript note) <<&>> \stderr ->
    MkNotifyException note AppleScript (decodeUtf8Lenient stderr)
  where
    notify =
      fmap exitFailureToStderr
        . P.readProcessStderr
        . P.shell
        . T.unpack

shrunToAppleScript :: ShrunNote -> Text
shrunToAppleScript shrunNote = txt
  where
    txt =
      mconcat
        [ "osascript -e 'display notification ",
          withDoubleQuotes (shrunNote ^. #body % #unUnlinedText),
          " with title \"Shrun\" ",
          " subtitle ",
          withDoubleQuotes (shrunNote ^. #summary % #unUnlinedText),
          "'"
        ]

withDoubleQuotes :: Text -> Text
withDoubleQuotes s = " \"" <> s <> "\" "
