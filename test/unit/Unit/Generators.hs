-- | Provides various generators for property tests.
module Unit.Generators
  ( genNonNegative,
    genPositive,
    genTimeSpec,
    genInt,
    genText,
    genUnlinedText,
    getNonEmptyText,
  )
where

import Effectful.Time.Dynamic (TimeSpec (MkTimeSpec))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Shrun.Data.Text (UnlinedText)
import Shrun.Data.Text qualified as ShrunText
import Unit.Prelude

-- | Generates 'NonNegative' in [0, 1_000_000].
genNonNegative :: Gen Natural
genNonNegative = Gen.integral (Range.constant 0 1_000_000)

-- | Generates 'Positive' in [1, 1_000_000].
genPositive :: Gen Natural
genPositive = Gen.integral (Range.constant 1 1_000_000)

-- | Generates 'TimeSpec' where 'sec' and 'nsec' are random 'Int64'.
genTimeSpec :: Gen TimeSpec
genTimeSpec = do
  MkTimeSpec
    <$> Gen.integral (Range.linearFrom 0 0 100)
    <*> Gen.integral (Range.linearFrom 1_000_000_000 0 100_000_000_000)

-- | Generates 'Int' based on 'Bounded'.
genInt :: Gen Int
genInt =
  let range = Range.linearFrom 0 minBound maxBound
   in Gen.int range

-- | Generates latin1 'Text' with 0-30 characters.
genText :: Gen Text
genText = Gen.text range Gen.latin1
  where
    range = Range.linearFrom 0 0 30

genUnlinedText :: Gen UnlinedText
genUnlinedText = ShrunText.fromTextReplace <$> genText

-- | Generates latin1 text with 1-30 characters.
getNonEmptyText :: Gen Text
getNonEmptyText = Gen.text range Gen.latin1
  where
    range = Range.linearFrom 1 1 30
