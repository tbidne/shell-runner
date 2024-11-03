{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | Effect for DBus.
module Shrun.Notify.DBus
  ( -- * Effect
    DBus (..),
    connectSession,
    notify,

    -- * Handler
    runDBus,

    -- * Functions
    notifyDBus,
  )
where

import DBus.Client (Client)
import DBus.Client qualified as DBusC
import DBus.Notify (Hint (Urgency), Note)
import DBus.Notify qualified as DBusN
import Data.Text qualified as T
import Shrun.Configuration.Data.Notify.System
  ( NotifySystemP (DBus),
    OsxNotifySystemMismatch (OsxNotifySystemMismatchDBus),
  )
import Shrun.Configuration.Data.Notify.Timeout
  ( NotifyTimeout
      ( NotifyTimeoutNever,
        NotifyTimeoutSeconds
      ),
  )
import Shrun.Notify.Effect (NotifyException (MkNotifyException), ShrunNote)
import Shrun.Prelude

data DBus :: Effect where
  ConnectSession :: DBus m Client
  Notify :: Client -> Note -> DBus m (Maybe SomeException)

-- | @since 0.1
type instance DispatchOf DBus = Dynamic

connectSession ::
  ( DBus :> es,
    HasCallStack
  ) =>
  Eff es Client
connectSession = send ConnectSession

notify ::
  ( DBus :> es,
    HasCallStack
  ) =>
  Client ->
  Note ->
  Eff es (Maybe SomeException)
notify c = send . Notify c

runDBus :: (HasCallStack, IOE :> es) => Eff (DBus : es) a -> Eff es a
runDBus = interpret_ $ \case
  ConnectSession -> liftIO DBusC.connectSession
  Notify client note ->
    trySync (liftIO $ DBusN.notify client note) <&> \case
      Left err -> Just err
      Right _ -> Nothing

notifyDBus ::
  ( DBus :> es,
    HasCallStack
  ) =>
  Client ->
  ShrunNote ->
  Eff es (Maybe NotifyException)
#if OSX
notifyDBus _ _ = throwM OsxNotifySystemMismatchDBus
#else
notifyDBus client note =
  notify client (shrunToDBus note) <<&>> \stderr ->
    MkNotifyException note (DBus ()) (T.pack $ displayException stderr)

shrunToDBus :: ShrunNote -> Note
shrunToDBus shrunNote =
  DBusN.Note
    { appName = "Shrun",
      summary = unpack $ shrunNote ^. #summary % #unUnlinedText,
      body = Just . DBusN.Text . T.unpack $ shrunNote ^. #body % #unUnlinedText,
      appImage = Nothing,
      hints = [Urgency (shrunNote ^. #urgency)],
      expiry,
      actions = []
    }
  where
    expiry = case shrunNote ^. #timeout of
      NotifyTimeoutNever -> DBusN.Never
      NotifyTimeoutSeconds s ->
        DBusN.Milliseconds $ 1_000 * unsafeConvertIntegral s
#endif
