-- | Property tests for ShellRun.Parsing.Commands.
module Props.ShellRun.Parsing.Commands
  ( props,
  )
where

import Data.Functor.Identity (Identity)
import Data.HashMap.Strict qualified as Map
import Data.HashSet (HashSet)
import Data.HashSet qualified as Set
import Hedgehog (GenBase, MonadGen, PropertyT)
import Hedgehog qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import MaxRuns (MaxRuns (..))
import ShellRun.Data.Command qualified as Command
import ShellRun.Data.Legend (LegendMap)
import ShellRun.Parsing.Commands qualified as ParseCommands
import ShellRun.Prelude
import Test.Tasty (TestTree)
import Test.Tasty qualified as T
import Test.Tasty.Hedgehog qualified as TH

-- | Entry point for ShellRun.Parsing.Commands property tests.
props :: TestTree
props = T.testGroup "ShellRun.Parsing.Commands" [translateProps]

translateProps :: TestTree
translateProps = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty "translateCommands includes everything" $
    H.withTests limit $
      H.property $ do
        (legend, origCmds) <- H.forAll genLegendCommands
        let legendKeySet = Set.fromList $ Map.keys legend
            maybeFinalCmds = ParseCommands.translateCommands legend origCmds

        case maybeFinalCmds of
          Left err -> do
            H.footnote $ "Received a LegendErr: " <> show err
            H.failure
          Right finalCmds -> do
            let finalCmdsSet = Set.fromList $ fmap Command.command finalCmds
                combinedKeySet = Set.union legendKeySet finalCmdsSet

            H.footnote $ "Final commands: " <> show finalCmdsSet
            H.footnote $ "Legend: " <> show legendKeySet
            noCommandsMissing combinedKeySet origCmds

-- Verify all of our original commands exist in the union:
--   LegendKeys \cup FinalCommands
noCommandsMissing :: HashSet Text -> List Text -> PropertyT IO ()
noCommandsMissing allKeys = void . traverse failIfMissing
  where
    failIfMissing cmd
      | Set.member cmd allKeys = pure ()
      | otherwise = do
          H.footnote $ "Missing command: " <> show cmd
          H.failure

genLegendCommands :: (GenBase m ~ Identity, MonadGen m) => m (LegendMap, List Text)
genLegendCommands = (,) <$> genLegend <*> genCommands

-- WARN: This can technically generate a map that has cycles in it,
-- e.g., a -> b -> c -> a, which would cause an infinite loop if
-- we also happen to generate a command in that cycle. The odds of this
-- happening have to be really low, so not worrying about this for now...
--
-- Update: Well, the "low probability event" happened (*gasp*). We generated
-- a cycle, in particular, the cycle a -> a with command a. That is, we
-- a map with key=key and command key. The good news is our cycle detection
-- works! We did not cause an infinite loop, merely a test assertion failure
-- (yay?).
--
-- We can also (weakly) justify the "low probability" as this is the first
-- time this error has been seen despite this test running 1000s of times
-- (indeed even running the test 100_000 times fails to reproduce it).
-- Nevertheless, we have added mitigation logic that excludes a=a key/value
-- mappings. Now the only possible cycles are non-trivial, surely an even
-- Lower Probability Event TM.
--
-- This seems an acceptable stopgap until we implement a robust solution.
-- It shouldn't actually be hard; just keep track of generated keys and
-- prevent values from reusing a key.
genLegend :: (GenBase m ~ Identity, MonadGen m) => m LegendMap
genLegend = Map.fromList <$> Gen.list range genKeyVal
  where
    range = Range.linearFrom 0 0 80

genKeyVal :: (GenBase m ~ Identity, MonadGen m) => m (Tuple2 Text Text)
genKeyVal = do
  k <- genKey
  -- TODO: This mitigates cycles, but obviously we'd like to eliminate
  -- the possibility. We should generate our list in the same function
  -- and prevent values from matching keys.
  v <- Gen.filter (/= k) genVal
  pure (k, v)

genKey :: MonadGen m => m Text
genKey = Gen.text range Gen.latin1
  where
    range = Range.linearFrom 1 1 10

genVal :: MonadGen m => m Text
genVal = Gen.text range Gen.latin1
  where
    range = Range.linearFrom 1 1 50

genCommands :: MonadGen m => m (List Text)
genCommands = Gen.list range genCommand
  where
    range = Range.linearFrom 1 1 50

genCommand :: MonadGen m => m Text
genCommand = Gen.text range Gen.latin1
  where
    range = Range.linearFrom 1 1 50
