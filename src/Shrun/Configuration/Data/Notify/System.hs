{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides type for notifications.
module Shrun.Configuration.Data.Notify.System
  ( -- * Notify system
    NotifySystemP (..),
    NotifySystemArgs,
    NotifySystemToml,
    NotifySystemMerged,
    NotifySystemEnv,
    parseNotifySystem,
    notifySystemStr,
    showNotifySystem,
    displayNotifySystem,
    DBusF,
    mergeNotifySystem,

    -- * Exceptions
    OsxNotifySystemMismatch (..),
    LinuxNotifySystemMismatch (..),
  )
where

import DBus.Client (Client)
import Shrun.Configuration.Data.ConfigPhase
  ( ConfigPhase
      ( ConfigPhaseArgs,
        ConfigPhaseEnv,
        ConfigPhaseMerged,
        ConfigPhaseToml
      ),
  )
import Shrun.Configuration.Data.WithDisabled
  ( WithDisabled (Disabled, With, Without),
  )
import Shrun.Configuration.Default (Default (def))
import Shrun.Prelude
import Shrun.Utils qualified as Utils

-- | Maps DBus to its phased param.
type DBusF :: ConfigPhase -> Type
type family DBusF p where
  DBusF ConfigPhaseArgs = ()
  DBusF ConfigPhaseToml = ()
  DBusF ConfigPhaseMerged = ()
  DBusF ConfigPhaseEnv = Client

type NotifySystemArgs = NotifySystemP ConfigPhaseArgs

type NotifySystemToml = NotifySystemP ConfigPhaseToml

type NotifySystemMerged = NotifySystemP ConfigPhaseMerged

type NotifySystemEnv = NotifySystemP ConfigPhaseEnv

-- | Notification systems.
type NotifySystemP :: ConfigPhase -> Type
data NotifySystemP p
  = -- | Uses DBus.
    DBus (DBusF p)
  | -- | Uses notify-send.
    NotifySend
  | -- | Uses apple-script.
    AppleScript

deriving stock instance Eq NotifySystemArgs

deriving stock instance Show NotifySystemArgs

deriving stock instance Eq NotifySystemToml

deriving stock instance Show NotifySystemToml

deriving stock instance Eq NotifySystemMerged

deriving stock instance Show NotifySystemMerged

-- | "Merges" notify systems.
mergeNotifySystem ::
  WithDisabled NotifySystemArgs ->
  Maybe NotifySystemToml ->
  NotifySystemMerged
mergeNotifySystem mArgs mToml =
  case mArgs of
    Disabled -> def
    With (DBus ()) -> DBus ()
    With NotifySend -> NotifySend
    With AppleScript -> AppleScript
    Without -> case mToml of
      Just (DBus ()) -> DBus ()
      Just NotifySend -> NotifySend
      Just AppleScript -> AppleScript
      Nothing -> def

showNotifySystem :: (IsString a) => NotifySystemP p -> a
showNotifySystem (DBus _) = "DBus"
showNotifySystem NotifySend = "NotifySend"
showNotifySystem AppleScript = "AppleScript"

displayNotifySystem :: (IsString a) => NotifySystemP p -> a
displayNotifySystem (DBus _) = "dbus"
displayNotifySystem NotifySend = "notify-send"
displayNotifySystem AppleScript = "apple-script"

instance DecodeTOML NotifySystemToml where
  tomlDecoder = parseNotifySystem tomlDecoder

-- | Parses 'NotifySystem'.
parseNotifySystem :: (DBusF p ~ (), MonadFail m) => m Text -> m (NotifySystemP p)
parseNotifySystem getTxt =
  getTxt >>= \case
    "dbus" -> pure $ DBus ()
    "notify-send" -> pure NotifySend
    "apple-script" -> pure AppleScript
    bad ->
      fail
        $ Utils.fmtUnrecognizedError
          "notify system"
          notifySystemStr
          (unpack bad)

-- | Available 'NotifySystem' strings.
notifySystemStr :: (IsString a) => a
notifySystemStr = "(dbus | notify-send | apple-script)"

#if OSX
instance Default (NotifySystemP p) where
  def = AppleScript
#else
instance (DBusF p ~ ()) => Default (NotifySystemP p) where
  def = DBus ()
#endif

data OsxNotifySystemMismatch
  = OsxNotifySystemMismatchDBus
  | OsxNotifySystemMismatchNotifySend
  deriving stock (Eq, Show)

instance Exception OsxNotifySystemMismatch where
  displayException OsxNotifySystemMismatchDBus =
    "Detected osx, but DBus is only available on linux!"
  displayException OsxNotifySystemMismatchNotifySend =
    "Detected osx, but NotifySend is only available on linux!"

data LinuxNotifySystemMismatch = LinuxNotifySystemMismatchAppleScript
  deriving stock (Eq, Show)

instance Exception LinuxNotifySystemMismatch where
  displayException LinuxNotifySystemMismatchAppleScript =
    "Detected linux, but AppleScript is only available on osx!"
