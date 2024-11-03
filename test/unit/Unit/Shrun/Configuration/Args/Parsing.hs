{-# LANGUAGE QuasiQuotes #-}

-- | Tests for Shrun.Args
module Unit.Shrun.Configuration.Args.Parsing (tests) where

import Data.Sequence qualified as Seq
import Data.Text qualified as T
import Options.Applicative qualified as OA
import Shrun.Configuration.Args (defaultArgs)
import Shrun.Utils qualified as U
import Unit.Prelude
import Unit.Shrun.Configuration.Args.Parsing.Core qualified as Core
import Unit.Shrun.Configuration.Args.Parsing.TestUtils qualified as U

-- | Entry point for Shrun.Args specs.
tests :: TestTree
tests =
  testGroup
    "Shrun.Configuration.Args.Parsing"
    [ defaultTests,
      configTests,
      Core.tests,
      commandTests,
      testDefaultOption
    ]

defaultTests :: TestTree
defaultTests =
  testGroup
    "Defaults"
    [testDefaultArgs]

testDefaultArgs :: TestTree
testDefaultArgs = testPropertyNamed desc "testDefaultArgs" $ do
  let argList = ["command"]
      expected = Just $ defaultArgs ("command" :<|| Seq.empty)
  U.verifyResult argList expected
  where
    desc = "Parses default args"

configTests :: TestTree
configTests =
  testGroup
    "--config"
    [ testConfigShort,
      testConfig,
      testNoConfig
    ]

testConfigShort :: TestTree
testConfigShort =
  testPropertyNamed "Parses -c" "testConfigShort"
    $ U.verifyResult argList expected
  where
    argList = ["-c./path/config.toml", "command"]
    expected = U.updateDefArgs #configPath [osp|./path/config.toml|]

testConfig :: TestTree
testConfig =
  testPropertyNamed "Parses --config" "testConfig"
    $ U.verifyResult argList expected
  where
    argList = ["--config=./path/config.toml", "command"]
    expected = U.updateDefArgs #configPath [osp|./path/config.toml|]

testNoConfig :: TestTree
testNoConfig =
  testPropertyNamed "Parses --no-config" "testNoConfig"
    $ U.verifyResult argList expected
  where
    argList = ["--no-config", "command"]
    expected = U.disableDefArgs #configPath

commandTests :: TestTree
commandTests =
  testGroup
    "Commands"
    [ emptyCommandsFail,
      testCommands
    ]

emptyCommandsFail :: TestTree
emptyCommandsFail =
  testPropertyNamed "Empty commands fail" "emptyCommandsFail"
    $ U.verifyFailure []

testCommands :: TestTree
testCommands =
  testPropertyNamed "Bare strings parsed as commands" "testCommands"
    $ U.verifyResult argList expected
  where
    argList = ["one", "two", "three"]
    expected = ((_Just % #commands) .~ cmds) U.defArgs
    cmds = U.unsafeListToNESeq ["one", "two", "three"]

testDefaultOption :: TestTree
testDefaultOption = testPropertyNamed desc "testDefaultOption"
  $ withTests 1
  $ property
  $ do
    expected <-
      liftIO
        $ runEff
        $ runFileReader
        $ readFileUtf8ThrowM [osp|examples/default.toml|]

    let result = U.execParserUnit ["--default-config"]

    case result of
      OA.Success x -> do
        annotateShow x
        failure
      OA.Failure f -> do
        let expectedLines = T.lines expected
            expectedLen = length expectedLines
            resultLines = T.lines $ pack $ fst (OA.renderFailure f "")
            resultLen = length resultLines

        annotateShow expectedLines
        annotateShow resultLines

        expectedLen === resultLen

        for_ (zip expectedLines resultLines) $ \(e, r) -> do
          e === r
      OA.CompletionInvoked x -> do
        annotateShow x
        failure
  where
    desc = "--default-config === examples/default.toml"
