{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | Transforms NotifyToml into NotifyEnv.
module Shrun.Configuration.Env.Notify
  ( tomlToNotifyEnv,
  )
where

import Effects.Exception (throwString)
import Shrun.Configuration.Env.Types (NotifyEnv (..))
import Shrun.Configuration.Toml (NotifyToml (..))
import Shrun.Data.Phase (AdvancePhase (advancePhase))
import Shrun.Notify.MonadDBus (MonadDBus (connectSession))
import Shrun.Notify.Types
  ( NotifyAction (..),
    NotifySystem (..),
    NotifySystemP1,
    NotifyTimeout (..),
    _AppleScript,
    _DBus,
    _NotifySend,
  )
import Shrun.Prelude

{- ORMOLU_DISABLE -}

-- | Transforms NotifyToml into NotifyEnv. Notifications are off if one of
-- the following is true:
--
-- 1. NotifyToml is completely unspecified (i.e. Nothing)
-- 2. NotifyNone is specified
tomlToNotifyEnv ::
  ( HasCallStack,
    MonadDBus m,
    MonadThrow m
  ) =>
  Maybe NotifyToml ->
  m (Maybe NotifyEnv)
tomlToNotifyEnv Nothing = pure Nothing
tomlToNotifyEnv (Just notifyToml)
#if OSX
  | is (#system %? _DBus) notifyToml = throwString "DBus is only available on linux!"
  | is (#system %? _NotifySend) notifyToml = throwString "NotifySend is only available on linux!"
#else
  | is (#system %? _AppleScript) notifyToml = throwString "AppleScript is only available on osx!"
#endif
  | otherwise = case advancePhase systemP1 of
    Left sys -> pure $ Just $ mkNotify sys
    Right mkDBus -> Just . mkNotify . mkDBus <$> connectSession
  where
#if OSX
    systemP1 = fromMaybe AppleScript (notifyToml ^. #system)
#else
    systemP1 = fromMaybe (DBus ()) (notifyToml ^. #system)
#endif
    mkNotify systemP2 =
      MkNotifyEnv
        { system = systemP2,
          action = notifyToml ^. #action,
          timeout = fromMaybe (NotifyTimeoutSeconds 10) (notifyToml ^. #timeout)
        }

{- ORMOLU_ENABLE -}
