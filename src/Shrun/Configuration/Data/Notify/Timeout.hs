-- | Provides type for notifications.
module Shrun.Configuration.Data.Notify.Timeout
  ( NotifyTimeout (..),
    parseNotifyTimeout,
    notifyTimeoutStr,
  )
where

import Data.Bits (toIntegralSized)
import Data.Word (Word16)
import GHC.Num (Num (fromInteger))
import Shrun.Configuration.Default (Default (def))
import Shrun.Prelude
import Shrun.Utils qualified as U
import Shrun.Utils qualified as Utils
import TOML (Value (Integer, String))

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
      Nothing -> invalidValue (tooLargeErr Nothing) (Integer i)
    badTy -> typeMismatch badTy
    where
      strErr = "Unexpected timeout. Only valid string is 'never'."

tooLargeErr :: Maybe Integer -> Text
tooLargeErr Nothing = "Timeout integer too large. Max is: " <> showt maxW16
tooLargeErr (Just i) =
  mconcat
    [ "Timeout integer '",
      showt i,
      "' too large. Max is: ",
      showt maxW16
    ]

maxW16 :: Word16
maxW16 = maxBound

-- | Parses 'NotifyTimeout'.
parseNotifyTimeout :: (MonadFail m) => m Text -> m NotifyTimeout
parseNotifyTimeout getTxt =
  getTxt >>= \case
    "never" -> pure NotifyTimeoutNever
    other -> case U.readStripUnderscores @_ @Integer other of
      Just nInt -> case toIntegralSized nInt of
        Just nW16 -> pure $ NotifyTimeoutSeconds nW16
        Nothing -> fail (unpack $ tooLargeErr (Just nInt))
      Nothing ->
        fail
          $ Utils.fmtUnrecognizedError
            "notify timeout"
            notifyTimeoutStr
            (unpack other)

-- | Available 'NotifyTimeout' strings.
notifyTimeoutStr :: (IsString a) => a
notifyTimeoutStr = "(never | NATURAL)"
