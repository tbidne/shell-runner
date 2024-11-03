{-# LANGUAGE UndecidableInstances #-}

-- | Provides effects for sending notifications.
module Shrun.Notify.Effect
  ( -- * Effect
    Notify (..),
    notify,
    ShrunNote (..),
    NotifyException (..),
    exitFailureToStderr,
  )
where

import DBus.Notify (UrgencyLevel)
import Data.ByteString.Lazy qualified as BSL
import Data.Text qualified as T
import Shrun.Configuration.Data.Notify.System
  ( NotifySystemMerged,
    displayNotifySystem,
  )
import Shrun.Configuration.Data.Notify.Timeout (NotifyTimeout)
import Shrun.Data.Text (UnlinedText)
import Shrun.Prelude

-- | Holds notification data.
data ShrunNote = MkShrunNote
  { body :: UnlinedText,
    summary :: UnlinedText,
    timeout :: NotifyTimeout,
    urgency :: UrgencyLevel
  }
  deriving stock (Eq, Show)

instance
  ( k ~ A_Lens,
    a ~ UnlinedText,
    b ~ UnlinedText
  ) =>
  LabelOptic "body" k ShrunNote ShrunNote a b
  where
  labelOptic =
    lensVL
      $ \f (MkShrunNote a1 a2 a3 a4) ->
        fmap
          (\b -> MkShrunNote b a2 a3 a4)
          (f a1)
  {-# INLINE labelOptic #-}

instance
  ( k ~ A_Lens,
    a ~ UnlinedText,
    b ~ UnlinedText
  ) =>
  LabelOptic "summary" k ShrunNote ShrunNote a b
  where
  labelOptic =
    lensVL
      $ \f (MkShrunNote a1 a2 a3 a4) ->
        fmap
          (\b -> MkShrunNote a1 b a3 a4)
          (f a2)
  {-# INLINE labelOptic #-}

instance
  ( k ~ A_Lens,
    a ~ NotifyTimeout,
    b ~ NotifyTimeout
  ) =>
  LabelOptic "timeout" k ShrunNote ShrunNote a b
  where
  labelOptic =
    lensVL
      $ \f (MkShrunNote a1 a2 a3 a4) ->
        fmap
          (\b -> MkShrunNote a1 a2 b a4)
          (f a3)
  {-# INLINE labelOptic #-}

instance
  ( k ~ A_Lens,
    a ~ UrgencyLevel,
    b ~ UrgencyLevel
  ) =>
  LabelOptic "urgency" k ShrunNote ShrunNote a b
  where
  labelOptic =
    lensVL
      $ \f (MkShrunNote a1 a2 a3 a4) ->
        fmap
          (\b -> MkShrunNote a1 a2 a3 b)
          (f a4)
  {-# INLINE labelOptic #-}

-- | Exception for sending desktop notifications.
data NotifyException = MkNotifyException ShrunNote NotifySystemMerged Text
  deriving stock (Eq, Show)

instance Exception NotifyException where
  displayException (MkNotifyException note system message) =
    mconcat
      [ "Exception sending notification with system '",
        displayNotifySystem system,
        "' and note '",
        show note,
        "': ",
        T.unpack message
      ]

-- | General effect for sending notifications.
data Notify :: Effect where
  Notify :: ShrunNote -> Notify m (Maybe NotifyException)

-- | @since 0.1
type instance DispatchOf Notify = Dynamic

notify ::
  ( HasCallStack,
    Notify :> es
  ) =>
  ShrunNote ->
  Eff es (Maybe NotifyException)
notify = send . Notify

-- | Maps (ExitCode, stderr) to Just stderr, if the exit code is
-- ExitFailure.
exitFailureToStderr :: (ExitCode, BSL.ByteString) -> Maybe ByteString
exitFailureToStderr (ex, stderr) = case ex of
  ExitSuccess -> Nothing
  ExitFailure _ -> Just (BSL.toStrict stderr)
