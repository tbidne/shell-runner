{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Property tests for Shrun.Logging.Formatting.
--
-- @since 0.1
module Unit.Props.Shrun.Logging.Formatting
  ( props,
  )
where

import Data.Functor.Identity (Identity (..))
import Data.Text qualified as T
import Hedgehog qualified as H
import Hedgehog.Gen qualified as HGen
import Hedgehog.Internal.Range qualified as HRange
import Shrun.Configuration.Env.Types
  ( CmdDisplay (..),
    HasLogging (..),
    StripControl (..),
    TruncRegion (..),
    Truncation (..),
  )
import Shrun.Data.Command (Command (..))
import Shrun.Logging.Formatting qualified as Formatting
import Shrun.Logging.Types (Log (..), LogLevel (..))
import Shrun.Logging.Types qualified as Log
import Test.Tasty qualified as T
import Unit.MaxRuns (MaxRuns (..))
import Unit.Prelude
import Unit.Props.Shrun.Logging.Generators qualified as LGens

data Env = MkEnv
  { cmdDisplay :: CmdDisplay,
    cmdTrunc :: Maybe (Truncation 'TCmdName),
    lineTrunc :: Maybe (Truncation 'TCmdLine)
  }
  deriving stock (Eq, Show)

makeFieldLabelsNoPrefix ''Env

genEnv :: Gen Env
genEnv =
  MkEnv
    <$> HGen.enumBounded
    <*> (fmap . fmap) MkTruncation genMInt
    <*> (fmap . fmap) MkTruncation genMInt

cmdTruncLimit :: Integral a => a
cmdTruncLimit = 30

genEnvCmdTrunc :: Gen Env
genEnvCmdTrunc =
  MkEnv
    <$> HGen.enumBounded
    <*> fmap (Just . MkTruncation) genNat
    <*> pure Nothing
  where
    genNat = HGen.integral range
    range = HRange.linear 0 cmdTruncLimit

genLongCmdText :: Gen Text
genLongCmdText = HGen.text range HGen.latin1
  where
    range = HRange.linearFrom (cmdTruncLimit + 1) (cmdTruncLimit + 1) 100

lineTruncLimit :: Integral a => a
lineTruncLimit = 80

genEnvLineTrunc :: Gen Env
genEnvLineTrunc =
  MkEnv
    <$> HGen.enumBounded
    <*> pure Nothing
    <*> fmap (Just . MkTruncation) genNat
  where
    genNat = HGen.integral range
    range = HRange.linear 0 lineTruncLimit

genLongLineText :: Gen Text
genLongLineText = HGen.text range HGen.latin1
  where
    range = HRange.linearFrom (lineTruncLimit + 1) (lineTruncLimit + 1) 120

genEnvDispCmd :: Gen Env
genEnvDispCmd =
  MkEnv HideKey
    <$> (fmap . fmap) MkTruncation genMInt
    <*> (fmap . fmap) MkTruncation genMInt

genEnvDispKey :: Gen Env
genEnvDispKey =
  MkEnv ShowKey
    <$> (fmap . fmap) MkTruncation genMInt
    <*> (fmap . fmap) MkTruncation genMInt

genMInt :: Gen (Maybe Natural)
genMInt = HGen.choice [Just <$> genNat, pure Nothing]
  where
    genNat = HGen.integral range
    range = HRange.exponential 0 100

runMockApp :: ReaderT env Identity a -> env -> a
runMockApp env = runIdentity . runReaderT env

instance HasLogging Env where
  getCmdDisplay = view #cmdDisplay
  getDisableLogging = const False
  getCmdLogging = const True
  getCmdLogStripControl = const $ Just StripControlNone
  getCmdLogNameTrunc = view #cmdTrunc
  getCmdLogLineTrunc = view #lineTrunc
  getFileLogging = const Nothing
  getFileLogStripControl = const $ Just StripControlAll

-- | Entry point for Shrun.Logging.Formatting property tests.
props :: TestTree
props =
  T.testGroup
    "Shrun.Logging.Formatting"
    [ messageProps,
      prefixProps,
      displayCmdProps,
      displayKeyProps,
      cmdTruncProps,
      lineTruncProps
    ]

messageProps :: TestTree
messageProps = T.askOption $ \(MkMaxRuns limit) ->
  testPropertyNamed "Includes message" "messageProps" $
    H.withTests limit $
      H.property $ do
        env <- H.forAll genEnv
        log@MkLog {msg} <- H.forAll LGens.genLog
        let result = runMockApp (Formatting.formatConsoleLog log) env
        H.annotate $ "Result: " <> T.unpack result
        H.assert $ T.strip msg `T.isInfixOf` result || "..." `T.isSuffixOf` result

prefixProps :: TestTree
prefixProps = T.askOption $ \(MkMaxRuns limit) ->
  testPropertyNamed "Formats prefix" "prefixProps" $
    H.withTests limit $
      H.property $ do
        env <- H.forAll genEnv
        log@MkLog {lvl} <- H.forAll LGens.genLog
        let result = runMockApp (Formatting.formatConsoleLog log) env
        H.annotate $ "Result: " <> T.unpack result
        case lvl of
          -- level is None: no prefix
          None -> foldr (noMatch result) (pure ()) nonEmptyPrefixes
          -- level is not None: includes prefix
          _ -> do
            let pfx = Log.levelToPrefix lvl
            H.annotate $ T.unpack pfx
            H.assert $ pfx `T.isInfixOf` result || "..." `T.isSuffixOf` result
  where
    nonEmptyPrefixes = [SubCommand .. Fatal]
    noMatch :: Text -> LogLevel -> PropertyT IO () -> PropertyT IO ()
    noMatch t level acc = do
      let pfx = Log.levelToPrefix level
      H.annotate $ T.unpack t
      H.annotate $ T.unpack pfx
      H.assert $ not (T.isInfixOf pfx t)
      acc

displayCmdProps :: TestTree
displayCmdProps = T.askOption $ \(MkMaxRuns limit) ->
  testPropertyNamed "Displays command literal" "displayCmdProps" $
    H.withTests limit $
      H.property $ do
        env <- H.forAll genEnvDispCmd
        log@MkLog {cmd = Just (MkCommand _ cmd')} <- H.forAll LGens.genLogWithCmd
        let result = runMockApp (Formatting.formatConsoleLog log) env
        H.annotate $ "Result: " <> T.unpack result
        H.assert $ cmd' `T.isInfixOf` result || "..." `T.isInfixOf` result

displayKeyProps :: TestTree
displayKeyProps = T.askOption $ \(MkMaxRuns limit) ->
  testPropertyNamed "Displays command lkey" "displayKeyProps" $
    H.withTests limit $
      H.property $ do
        env <- H.forAll genEnvDispKey
        log@MkLog {cmd = Just (MkCommand (Just key) _)} <- H.forAll LGens.genLogWithCmdKey
        let result = runMockApp (Formatting.formatConsoleLog log) env
        H.annotate $ "Result: " <> T.unpack result
        H.assert $ key `T.isInfixOf` result || "..." `T.isInfixOf` result

cmdTruncProps :: TestTree
cmdTruncProps = T.askOption $ \(MkMaxRuns limit) ->
  testPropertyNamed "Truncates long command" "cmdTruncProps" $
    H.withTests limit $
      H.property $ do
        env <- H.forAll genEnvCmdTrunc
        cmd' <- MkCommand Nothing <$> H.forAll genLongCmdText
        log <- H.forAll LGens.genLog
        let log' = log {cmd = Just cmd'}
            result = runMockApp (Formatting.formatConsoleLog log') env
        H.annotate $ "Result: " <> T.unpack result
        H.assert $ "...]" `T.isInfixOf` result

lineTruncProps :: TestTree
lineTruncProps = T.askOption $ \(MkMaxRuns limit) ->
  testPropertyNamed "Truncates long line" "lineTruncProps" $
    H.withTests limit $
      H.property $ do
        env <- H.forAll genEnvLineTrunc
        msg' <- H.forAll genLongLineText
        log <- H.forAll LGens.genLog

        -- only perform line truncation for SubCommand (also requires a command)
        let log' = log {msg = msg', cmd = Just (MkCommand (Just "") ""), lvl = SubCommand}
            result = runMockApp (Formatting.formatConsoleLog log') env

        H.annotate $ "Result: " <> T.unpack result
        H.assert $ "..." `T.isSuffixOf` result
        H.diff result (\t l -> T.length t < l + colorLen) lineTruncLimit

-- Colorization adds chars that the shell interprets as color commands.
-- This affects the length, so if we do anything that tests the length
-- of a line, this needs to be taken into account.
--
-- The colorization looks like: \ESC[<digits>m ... \ESC[0m, where digit is up
-- to 3 chars. Strictly speaking, System.Console.Pretty only appears to use
-- two digit colors, i.e. 9 total, and in fact we passed 1,000,000 tests using
-- 9. Still, the standard mentions up to 3 digits, so we will use that, giving
-- a total of 10. More info:
-- https://en.wikipedia.org/wiki/ANSI_escape_code#3-bit_and_4-bit
colorLen :: Int
colorLen = 10
