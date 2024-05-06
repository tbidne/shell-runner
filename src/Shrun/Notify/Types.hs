{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides type for notifications.
module Shrun.Notify.Types
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

    -- ** Optics
    _DBus,
    _NotifySend,
    _AppleScript,

    -- * Notify actions
    NotifyAction (..),
    parseNotifyAction,
    notifyActionStr,

    -- ** Optics
    _NotifyFinal,
    _NotifyCommand,

    -- * Notify timeout
    NotifyTimeout (..),
    parseNotifyTimeout,
    notifyTimeoutStr,

    -- * Exceptions
    OsxNotifySystemMismatch (..),
    LinuxNotifySystemMismatch (..),
  )
where

import DBus.Client (Client)
import Data.Bits (toIntegralSized)
import Data.String (IsString)
import Data.Text qualified as T
import Data.Word (Word16)
import GHC.Num (Num (fromInteger))
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
import TOML (Value (Integer, String))
import Text.Read qualified as TR

-- | Determines for which actions we should send notifications.
data NotifyAction
  = -- | Send a notification after all commands are completed.
    NotifyFinal
  | -- | Send notifications when each command completes.
    NotifyCommand
  | -- | NotifyFinal and NotifyCommand.
    NotifyAll
  deriving stock (Eq, Show)

makePrisms ''NotifyAction

instance DecodeTOML NotifyAction where
  tomlDecoder = parseNotifyAction tomlDecoder

-- | Parses 'NotifyAction'.
parseNotifyAction :: (MonadFail m) => m Text -> m NotifyAction
parseNotifyAction getTxt =
  getTxt >>= \case
    "final" -> pure NotifyFinal
    "command" -> pure NotifyCommand
    "all" -> pure NotifyAll
    other ->
      fail
        $ mconcat
          [ "Unrecognized notify action: '",
            T.unpack other,
            "'. Expected one of ",
            notifyActionStr
          ]

-- | Available 'NotifyAction' strings.
notifyActionStr :: (IsString a) => a
notifyActionStr = "(final |command | all)"

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

makePrisms ''NotifySystemP

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
    other ->
      fail
        $ mconcat
          [ "Unrecognized notify system: '",
            T.unpack other,
            "'. Expected one of ",
            notifySystemStr
          ]

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

-- | Determines notification timeout.
data NotifyTimeout
  = -- | Times out after the given seconds.
    NotifyTimeoutSeconds Word16
  | -- | Never times out.
    NotifyTimeoutNever
  deriving stock (Eq, Show)

instance Default NotifyTimeout where
  def = NotifyTimeoutSeconds 10

instance FromInteger NotifyTimeout where
  afromInteger = NotifyTimeoutSeconds . fromInteger

-- DecodeTOML instance does not reuse parseNotifyTimeout as we want to
-- enforce the integer type.

instance DecodeTOML NotifyTimeout where
  tomlDecoder = makeDecoder $ \case
    String "never" -> pure NotifyTimeoutNever
    String bad -> invalidValue strErr (String bad)
    Integer i -> case toIntegralSized i of
      Just i' -> pure $ NotifyTimeoutSeconds i'
      Nothing -> invalidValue tooLargeErr (Integer i)
    badTy -> typeMismatch badTy
    where
      tooLargeErr = "Timeout integer too large. Max is: " <> showt maxW16
      strErr = "Unexpected timeout. Only valid string is 'never'."
      maxW16 = maxBound @Word16

-- | Parses 'NotifyTimeout'.
parseNotifyTimeout :: (MonadFail m) => m Text -> m NotifyTimeout
parseNotifyTimeout getTxt =
  getTxt >>= \case
    "never" -> pure NotifyTimeoutNever
    other ->
      let otherStr = T.unpack other
       in case TR.readMaybe otherStr of
            Just n -> pure $ NotifyTimeoutSeconds n
            Nothing ->
              fail
                $ mconcat
                  [ "Unrecognized notify timeout: '",
                    otherStr,
                    "'. Expected one of ",
                    notifyTimeoutStr
                  ]

-- | Available 'NotifyTimeout' strings.
notifyTimeoutStr :: (IsString a) => a
notifyTimeoutStr = "(never | NAT)"

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
