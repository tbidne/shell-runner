{-# LANGUAGE UndecidableInstances #-}

-- | Provides the 'Timeout' type.
module Shrun.Configuration.Data.Core.Timeout
  ( Timeout (..),
    parseTimeout,
    parseTimeoutStr,
    timeoutStr,
  )
where

import Data.Time.Relative qualified as RT
import Shrun.Prelude
import Shrun.Utils qualified as Utils

-- | Represents a timeout, which is a non-negative integer.
newtype Timeout = MkTimeout
  { unTimeout :: Natural
  }
  deriving stock (Eq, Ord, Show)
  deriving (Num) via Natural

instance
  (k ~ An_Iso, a ~ Natural, b ~ Natural) =>
  LabelOptic "unTimeout" k Timeout Timeout a b
  where
  labelOptic = iso (\(MkTimeout x) -> x) MkTimeout
  {-# INLINE labelOptic #-}

instance DecodeTOML Timeout where
  tomlDecoder =
    parseTimeout tomlDecoder tomlDecoder

-- NOTE: [CLI vs. Toml Types]
--
-- Normally we'd want CLI and Toml to share the exact same parsing, however,
-- we should take advantage of Toml's types when possible. For instance,
-- here we want to allow parsing a numeric nat or a "time string".
--
-- The CLI has to make do with strings everywhere, hence parseTimeout makes sense.

parseTimeout :: (Alternative f, MonadFail f) => f Natural -> f Text -> f Timeout
parseTimeout getNat getTxt =
  (MkTimeout <$> getNat) <|> (getTxt >>= parseTimeoutStr)

parseTimeoutStr :: (MonadFail f) => Text -> f Timeout
parseTimeoutStr txt = case RT.fromString str of
  Right n -> pure $ MkTimeout $ RT.toSeconds n
  Left bad ->
    fail
      $ Utils.fmtUnrecognizedError
        "timeout"
        timeoutStr
        bad
  where
    str = unpack txt

timeoutStr :: String
timeoutStr = "(NATURAL | STRING)"
