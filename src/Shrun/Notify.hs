{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Module for sending notifications.
module Shrun.Notify
  ( -- * Primary
    sendNotif,

    -- * Handler
    runNotify,
  )
where

import DBus.Notify (UrgencyLevel)
import Shrun.Configuration.Data.Notify.System (NotifySystemP (AppleScript, DBus, NotifySend))
import Shrun.Configuration.Env.Types
  ( Env,
    HasAnyError,
    HasCommonLogging,
    HasConsoleLogging,
    HasFileLogging,
    HasNotifyConfig (getNotifyConfig),
    setAnyErrorTrue,
  )
import Shrun.Data.Text (UnlinedText)
import Shrun.Data.Text qualified as ShrunText
import Shrun.Logging qualified as Logging
import Shrun.Logging.RegionLogger (RegionLogger, withRegion)
import Shrun.Logging.Types
  ( Log (MkLog, cmd, lvl, mode, msg),
    LogLevel (LevelError),
    LogMode (LogModeFinish),
  )
import Shrun.Notify.AppleScript qualified as AppleScript
import Shrun.Notify.DBus (DBus)
import Shrun.Notify.DBus qualified as DBus
import Shrun.Notify.Effect
  ( Notify (Notify),
    ShrunNote
      ( MkShrunNote,
        body,
        summary,
        timeout,
        urgency
      ),
    notify,
  )
import Shrun.Notify.NotifySend qualified as NotifySend
import Shrun.Prelude

-- | Sends a notification if they are enabled (linux only). Logs any failed
-- sends.
sendNotif ::
  forall env r es.
  ( HasAnyError env,
    HasCallStack,
    HasCommonLogging env,
    HasConsoleLogging env r,
    HasFileLogging env,
    HasNotifyConfig env,
    Notify :> es,
    Reader env :> es,
    RegionLogger r :> es,
    Concurrent :> es,
    Time :> es
  ) =>
  -- | Notif summary
  UnlinedText ->
  -- | Notif body
  UnlinedText ->
  -- | Notif urgency
  UrgencyLevel ->
  Eff es ()
sendNotif summary body urgency = do
  cfg <- asks @env getNotifyConfig
  traverse_ notifyWithErrorLogging (cfg ^? (_Just % #timeout))
  where
    notifyWithErrorLogging timeout =
      notify (mkNote timeout) >>= \case
        Nothing -> pure ()
        Just notifyEx -> withRegion @r Linear (logEx notifyEx)

    logEx ex r = do
      -- set exit code
      setAnyErrorTrue @env
      Logging.putRegionLog @env r
        $ MkLog
          { cmd = Nothing,
            msg =
              "Could not send notification: "
                <> ShrunText.fromTextReplace (pack (displayException ex)),
            lvl = LevelError,
            mode = LogModeFinish
          }

    mkNote timeout =
      MkShrunNote
        { summary,
          body,
          urgency,
          timeout
        }

runNotify ::
  forall r es a.
  ( DBus :> es,
    HasCallStack,
    Reader (Env r) :> es,
    TypedProcess :> es
  ) =>
  Eff (Notify : es) a ->
  Eff es a
runNotify = interpret_ $ \case
  Notify note -> do
    asks @(Env r) (preview (#config % #notify %? #system)) >>= \case
      Nothing -> pure Nothing
      Just nenv -> sendNote nenv
    where
      sendNote (DBus client) = DBus.notifyDBus client note
      sendNote NotifySend = NotifySend.notifyNotifySend note
      sendNote AppleScript = AppleScript.notifyAppleScript note
