{-# LANGUAGE UndecidableInstances #-}

-- | Provides the 'PollInterval' type.
module Shrun.Configuration.Data.CommandLogging.PollInterval
  ( PollInterval (..),
    parsePollInterval,
  )
where

import Shrun.Configuration.Default (Default (def))
import Shrun.Prelude

-- | Represents how often to poll for command logs, in microseconds.
-- Zero is interpreted as infinite i.e. limited only by the CPU.
newtype PollInterval = MkPollInterval {unPollInterval :: Natural}
  deriving stock (Eq, Ord, Show)
  deriving (Num) via Natural

instance
  (k ~ An_Iso, a ~ Natural, b ~ Natural) =>
  LabelOptic "unPollInterval" k PollInterval PollInterval a b
  where
  labelOptic = iso (\(MkPollInterval x) -> x) MkPollInterval
  {-# INLINE labelOptic #-}

instance DecodeTOML PollInterval where
  tomlDecoder = MkPollInterval <$> tomlDecoder

parsePollInterval :: (Functor m) => m Natural -> m PollInterval
parsePollInterval getNat = MkPollInterval <$> getNat

instance Default PollInterval where
  def = MkPollInterval 10_000
