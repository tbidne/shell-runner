{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Shrun.Configuration.Data.Notify
  ( NotifyP (..),
    NotifyArgs,
    NotifyToml,
    NotifyMerged,
    NotifyEnv,
    mergeNotifyLogging,
    toEnv,
  )
where

import Shrun.Configuration.Data.ConfigPhase
  ( ConfigPhase
      ( ConfigPhaseArgs,
        ConfigPhaseEnv,
        ConfigPhaseMerged,
        ConfigPhaseToml
      ),
    ConfigPhaseF,
  )
import Shrun.Configuration.Data.Notify.Action
  ( NotifyAction,
  )
import Shrun.Configuration.Data.Notify.System
  ( LinuxNotifySystemMismatch (LinuxNotifySystemMismatchAppleScript),
    NotifySystemEnv,
    NotifySystemP (AppleScript, DBus, NotifySend),
    OsxNotifySystemMismatch
      ( OsxNotifySystemMismatchDBus,
        OsxNotifySystemMismatchNotifySend
      ),
    displayNotifySystem,
    mergeNotifySystem,
  )
import Shrun.Configuration.Data.Notify.Timeout
  ( NotifyTimeout,
  )
import Shrun.Configuration.Data.WithDisabled
  ( WithDisabled (Disabled, With, Without),
    (<.>?),
    (<>?),
  )
import Shrun.Configuration.Data.WithDisabled qualified as WD
import Shrun.Configuration.Default (Default, def)
import Shrun.Notify.DBus (DBus, connectSession)
import Shrun.Prelude

-- See NOTE: [Args vs. Toml mandatory fields]

-- | Notify action is mandatory if we are running notifications.
type NotifyActionF :: ConfigPhase -> Type
type family NotifyActionF p where
  NotifyActionF ConfigPhaseArgs = WithDisabled NotifyAction
  NotifyActionF ConfigPhaseToml = NotifyAction
  NotifyActionF ConfigPhaseMerged = NotifyAction
  NotifyActionF ConfigPhaseEnv = NotifyAction

-- | Holds notification config.
type NotifyP :: ConfigPhase -> Type
data NotifyP p = MkNotifyP
  { -- | Actions for which to send notifications.
    action :: NotifyActionF p,
    -- | The notification system to use.
    system :: ConfigPhaseF p (NotifySystemP p),
    -- | when to timeout successful notifications.
    timeout :: ConfigPhaseF p NotifyTimeout
  }

instance
  ( k ~ A_Lens,
    a ~ NotifyActionF p,
    b ~ NotifyActionF p
  ) =>
  LabelOptic "action" k (NotifyP p) (NotifyP p) a b
  where
  labelOptic =
    lensVL
      $ \f (MkNotifyP a1 a2 a3) ->
        fmap
          (\b -> MkNotifyP b a2 a3)
          (f a1)
  {-# INLINE labelOptic #-}

instance
  ( k ~ A_Lens,
    a ~ ConfigPhaseF p (NotifySystemP p),
    b ~ ConfigPhaseF p (NotifySystemP p)
  ) =>
  LabelOptic "system" k (NotifyP p) (NotifyP p) a b
  where
  labelOptic =
    lensVL
      $ \f (MkNotifyP a1 a2 a3) ->
        fmap
          (\b -> MkNotifyP a1 b a3)
          (f a2)
  {-# INLINE labelOptic #-}

instance
  ( k ~ A_Lens,
    a ~ ConfigPhaseF p NotifyTimeout,
    b ~ ConfigPhaseF p NotifyTimeout
  ) =>
  LabelOptic "timeout" k (NotifyP p) (NotifyP p) a b
  where
  labelOptic =
    lensVL
      $ \f (MkNotifyP a1 a2 a3) ->
        fmap
          (\b -> MkNotifyP a1 a2 b)
          (f a3)
  {-# INLINE labelOptic #-}

type NotifyArgs = NotifyP ConfigPhaseArgs

type NotifyToml = NotifyP ConfigPhaseToml

type NotifyMerged = NotifyP ConfigPhaseMerged

type NotifyEnv = NotifyP ConfigPhaseEnv

deriving stock instance Eq (NotifyP ConfigPhaseArgs)

deriving stock instance Show (NotifyP ConfigPhaseArgs)

deriving stock instance Eq (NotifyP ConfigPhaseToml)

deriving stock instance Show (NotifyP ConfigPhaseToml)

deriving stock instance Eq (NotifyP ConfigPhaseMerged)

deriving stock instance Show (NotifyP ConfigPhaseMerged)

-- Only Default instance is for Args, since others require the action.
instance Default NotifyArgs where
  def =
    MkNotifyP
      { system = def,
        action = def,
        timeout = def
      }

-- | Merges args and toml configs.
mergeNotifyLogging ::
  NotifyArgs ->
  Maybe NotifyToml ->
  Maybe NotifyMerged
mergeNotifyLogging args mToml =
  mAction <&> \action ->
    let toml :: NotifyToml
        toml = fromMaybe (defaultNotifyToml action) mToml
     in MkNotifyP
          { action,
            system =
              mergeNotifySystem (args ^. #system) (toml ^. #system),
            timeout =
              (args ^. #timeout) <.>? (toml ^. #timeout)
          }
  where
    mAction = case (args ^. #action, mToml) of
      -- 1. Logging globally disabled
      (Disabled, _) -> Nothing
      -- 2. No Args and no Toml
      (Without, Nothing) -> Nothing
      (With p, _) -> Just p
      (_, Just toml) -> Just $ toml ^. #action

instance DecodeTOML NotifyToml where
  tomlDecoder =
    MkNotifyP
      <$> getFieldWith tomlDecoder "action"
      <*> getFieldOptWith tomlDecoder "system"
      <*> getFieldOptWith tomlDecoder "timeout"

toEnv ::
  ( DBus :> es,
    HasCallStack
  ) =>
  NotifyMerged ->
  Eff es NotifyEnv

#if OSX

toEnv notifyMerged = case systemMerged of
  DBus _ -> throwM OsxNotifySystemMismatchDBus
  NotifySend -> throwM OsxNotifySystemMismatchNotifySend
  AppleScript -> pure $ mkNotify notifyMerged AppleScript
  where
    systemMerged = notifyMerged ^. #system

#else

toEnv notifyMerged = case systemMerged of
  AppleScript -> throwM LinuxNotifySystemMismatchAppleScript
  DBus _ -> mkNotify notifyMerged . DBus <$> connectSession
  NotifySend -> pure $ mkNotify notifyMerged NotifySend
  where
    systemMerged = notifyMerged ^. #system

#endif

mkNotify :: NotifyMerged -> NotifySystemEnv -> NotifyEnv
mkNotify notifyToml systemP2 =
  MkNotifyP
    { system = systemP2,
      action = notifyToml ^. #action,
      timeout = notifyToml ^. #timeout
    }

defaultNotifyToml :: NotifyAction -> NotifyToml
defaultNotifyToml action =
  MkNotifyP
    { system = Nothing,
      action = action,
      timeout = Nothing
    }
