{-# LANGUAGE UndecidableInstances #-}

module Shrun.Configuration.Data.Truncation
  ( TruncRegion (..),
    Truncation (..),
    parseTruncation,
    LineTruncation (..),
    parseLineTruncation,

    -- * Misc
    decodeCommandNameTrunc,
    decodeLineTrunc,
    configToLineTrunc,
    lineTruncStr,
  )
where

import Effectful.Terminal.Dynamic (getTerminalWidth)
import Shrun.Configuration.Data.WithDisabled
  ( WithDisabled
      ( Disabled,
        With,
        Without
      ),
  )
import Shrun.Prelude
import Shrun.Utils qualified as Utils

-- | The different regions to apply truncation rules.
data TruncRegion
  = -- | Apply truncation rules to commands/key names.
    TruncCommandName
  | -- | Apply truncation rules to command log entire lines.
    TruncLine
  deriving stock (Eq, Show)

-- | The maximum number of command characters to display in the logs.
type Truncation :: TruncRegion -> Type
newtype Truncation a = MkTruncation
  { unTruncation :: Int
  }
  deriving stock (Eq, Ord, Show)
  deriving (Num) via Int

instance
  (k ~ An_Iso, a ~ Int, b ~ Int) =>
  LabelOptic "unTruncation" k (Truncation r) (Truncation r) a b
  where
  labelOptic = iso (\(MkTruncation x) -> x) MkTruncation
  {-# INLINE labelOptic #-}

instance DecodeTOML (Truncation a) where
  tomlDecoder = parseTruncation tomlDecoder

parseTruncation :: (MonadFail m) => m Natural -> m (Truncation r)
parseTruncation getNat = do
  n <- getNat
  case convertIntegral n of
    Left err -> fail err
    Right x -> pure $ MkTruncation x

-- | Determines command log line truncation behavior. We need a separate
-- type from 'Truncation' to add a third option, to detect the terminal size
-- automatically.
data LineTruncation
  = Undetected (Truncation TruncLine)
  | Detected
  deriving stock (Eq, Show)

instance DecodeTOML LineTruncation where
  tomlDecoder = parseLineTruncation tomlDecoder tomlDecoder

parseLineTruncation ::
  (Alternative m, MonadFail m) =>
  m Natural ->
  m Text ->
  m LineTruncation
parseLineTruncation getNat getTxt =
  Undetected
    <$> parseTruncation getNat
    -- NOTE: [Detect second parser]
    <|> parseDetected getTxt

-- Because this parser is used second, its error message is what will be
-- displayed.
--
-- see NOTE: [Detect second parser]
parseDetected :: (MonadFail m) => m Text -> m LineTruncation
parseDetected getTxt =
  getTxt >>= \case
    "detect" -> pure Detected
    bad ->
      fail
        $ Utils.fmtUnrecognizedError
          "line truncation"
          lineTruncStr
          (unpack bad)

lineTruncStr :: String
lineTruncStr = "(NATURAL | detect)"

decodeCommandNameTrunc :: Decoder (Maybe (Truncation TruncCommandName))
decodeCommandNameTrunc = getFieldOptWith tomlDecoder "command-name-trunc"

decodeLineTrunc :: Decoder (Maybe LineTruncation)
decodeLineTrunc = getFieldOptWith tomlDecoder "line-trunc"

-- | Maps line trunc config to actual value.
configToLineTrunc ::
  ( HasCallStack,
    Terminal :> es
  ) =>
  WithDisabled LineTruncation ->
  Eff es (Maybe (Truncation TruncLine))
configToLineTrunc Disabled = pure Nothing
configToLineTrunc Without = pure Nothing
configToLineTrunc (With Detected) =
  -- We subtract one because otherwise we can fill the entire terminal with a
  -- log, which will automatically add a newline. The point of this option is
  -- to avoid multiple lines, hence the subtraction.
  Just . MkTruncation . (\x -> x - 1) <$> getTerminalWidth
configToLineTrunc (With (Undetected x)) = pure $ Just x
