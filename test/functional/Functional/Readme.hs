-- | Functional tests for readme examples.
module Functional.Readme (specs) where

import Functional.Prelude
import Test.Shrun.Verifier (ResultText (..))
import Test.Shrun.Verifier qualified as V

-- NOTE: If tests in this module fail, fix then update the README!

-- | Specs from readme
specs :: TestTree
specs =
  testGroup
    "README examples"
    [ gif,
      core,
      timeout,
      cmdlogOn,
      cmdlogOff,
      keyHideOn,
      keyHideOff,
      stripControlAll,
      stripControlNone,
      stripControlSmart,
      cmdNameTruncN,
      cmdLogLineTruncN,
      cmdLogLineTruncDetect
    ]

gif :: TestTree
gif =
  testCase "Runs gif example" $ do
    results <- fmap MkResultText <$> (readIORef =<< runAndGetLogs args)
    V.verifyExpected results expected
  where
    args =
      withBaseArgs
        [ "sign-peace-treaty",
          "takeover"
        ]
    expected =
      [ withFinishedPrefix "13 seconds",
        withSuccessPrefix "skynet",
        withSuccessPrefix "ui",
        withErrorPrefix "sign-peace-treaty" "5 seconds: /bin/sh: line 1: lol psyche: command not found",
        withCommandPrefix "skynet" "preparing nuclear missil-- i mean gift baskets",
        withCommandPrefix "ui" "adding emojis. we like to have fun :-)",
        withCommandPrefix "querying-targets" "finding targets...",
        withCommandPrefix "sign-peace-treaty" "play it cool...",
        withTimerPrefix "8 seconds"
      ]

core :: TestTree
core =
  testCase "Runs core example" $ do
    results <- fmap MkResultText <$> (readIORef =<< runAndGetLogs args)
    V.verifyExpected results expected
  where
    args =
      withBaseArgs
        [ "all",
          "echo cat"
        ]
    expected =
      [ withSuccessPrefix "echo cat",
        withSuccessPrefix "echo hi",
        withSuccessPrefix "cmd1",
        withErrorPrefix "cmd4" "0 seconds: /bin/sh: line 1: four: command not found",
        withFinishedPrefix "0 seconds"
      ]

timeout :: TestTree
timeout =
  testCase "Runs timeout example" $ do
    results <- fmap MkResultText <$> (readIORef =<< runAndGetLogs args)
    V.verifyExpected results expected
  where
    args =
      withNoConfig
        [ "-t",
          "8",
          "sleep 5",
          "sleep 10",
          "sleep 15"
        ]
    expected =
      [ withSuccessPrefix "sleep 5",
        withTimeoutPrefix "sleep 10, sleep 15",
        finishedPrefix
      ]

cmdlogOn :: TestTree
cmdlogOn =
  testCase "Runs cmdlog example with --cmd-log" $ do
    results <- fmap MkResultText <$> (readIORef =<< runAndGetLogs args)
    V.verifyExpected results expected
  where
    args =
      withNoConfig
        [ "--cmd-log",
          "for i in {1..10}; do echo hi; sleep 1; done"
        ]
    expected =
      [ withCommandPrefix "for i in {1..10}; do echo hi; sleep 1; done" "hi"
      ]

cmdlogOff :: TestTree
cmdlogOff =
  testCase "Runs cmdlog example without --cmd-log" $ do
    results <- fmap MkResultText <$> (readIORef =<< runAndGetLogs args)
    V.verifyUnexpected results unexpected
  where
    args =
      withNoConfig
        [ "for i in {1..10}; do echo hi; sleep 1; done"
        ]
    unexpected = [commandPrefix]

-- TODO: file logging

keyHideOn :: TestTree
keyHideOn =
  testCase "Runs key hide example with --key-hide" $ do
    results <- fmap MkResultText <$> (readIORef =<< runAndGetLogs args)
    V.verifyExpectedUnexpected results expected unexpected
  where
    args =
      withBaseArgs
        [ "--key-hide",
          "skynet"
        ]
    expected =
      [ withCommandPrefix
          "echo \"preparing nuclear missil-- i mean gift baskets\"; sleep 13"
          "preparing nuclear missil-- i mean gift baskets",
        withSuccessPrefix "echo \"preparing nuclear missil-- i mean gift baskets\"; sleep 13"
      ]
    unexpected =
      [ withCommandPrefix "skynet" "",
        withSuccessPrefix "skynet"
      ]

keyHideOff :: TestTree
keyHideOff =
  testCase "Runs key hide example without --key-hide" $ do
    results <- fmap MkResultText <$> (readIORef =<< runAndGetLogs args)
    V.verifyExpectedUnexpected results expected unexpected
  where
    args =
      withBaseArgs
        [ "skynet"
        ]
    expected =
      [ withCommandPrefix "skynet" "",
        withSuccessPrefix "skynet"
      ]
    unexpected =
      [ withCommandPrefix
          "echo \"preparing nuclear missil-- i mean gift baskets\"; sleep 13"
          "preparing nuclear missil-- i mean gift baskets",
        withSuccessPrefix "echo \"preparing nuclear missil-- i mean gift baskets\"; sleep 13"
      ]

stripControlAll :: TestTree
stripControlAll = testCase "Runs --cmd-log-strip-control all example" $ do
  results <- fmap MkResultText <$> (readIORef =<< runAndGetLogs args)
  V.verifyExpected results expected
  where
    args =
      withNoConfig
        [ "-lx10",
          "--cmd-log-strip-control",
          "all",
          "echo -e ' foo \ESC[35m hello \ESC[3D bye '; sleep 5"
        ]
    expected =
      [ withCommandPrefix "echo -e..." "foo  hello  bye"
      ]

stripControlNone :: TestTree
stripControlNone = testCase "Runs --cmd-log-strip-control none example" $ do
  results <- fmap MkResultText <$> (readIORef =<< runAndGetLogs args)
  V.verifyExpected results expected
  where
    args =
      withNoConfig
        [ "-lx10",
          "--cmd-log-strip-control",
          "none",
          "echo -e ' foo \ESC[35m hello \ESC[3D bye '; sleep 5"
        ]
    expected =
      [ withCommandPrefix "echo -e..." "foo \ESC[35m hello \ESC[3D bye"
      ]

stripControlSmart :: TestTree
stripControlSmart = testCase "Runs --cmd-log-strip-control smart example" $ do
  results <- fmap MkResultText <$> (readIORef =<< runAndGetLogs args)
  V.verifyExpected results expected
  where
    args =
      withNoConfig
        [ "-lx10",
          "--cmd-log-strip-control=smart",
          "echo -e ' foo \ESC[35m hello \ESC[3D bye '; sleep 5"
        ]
    expected =
      [ withCommandPrefix "echo -e..." "foo \ESC[35m hello  bye"
      ]

-- TODO: file log strip control

cmdNameTruncN :: TestTree
cmdNameTruncN = testCase "Runs --cmd-name-trunc 10 example" $ do
  results <- fmap MkResultText <$> (readIORef =<< runAndGetLogs args)
  V.verifyExpected results expected
  where
    args =
      withNoConfig
        [ "-l",
          "--cmd-name-trunc",
          "10",
          "for i in {1..3}; do echo hi; sleep 1; done"
        ]
    expected =
      [ withCommandPrefix "for i i..." "hi",
        withSuccessPrefix "for i i..."
      ]

cmdLogLineTruncN :: TestTree
cmdLogLineTruncN = testCase "Runs --cmd-log-line-trunc 80 example" $ do
  results <- fmap MkResultText <$> (readIORef =<< runAndGetLogs args)
  V.verifyExpected results expected
  where
    args =
      withNoConfig
        [ "-l",
          "--cmd-log-line-trunc",
          "80",
          "echo 'some ridiculously long command i mean is this really necessary' && sleep 5"
        ]
    expected =
      [ "[Command][echo 'some ridiculously long command i mean is this really necessar..."
      ]

cmdLogLineTruncDetect :: TestTree
cmdLogLineTruncDetect = testCase "Runs --cmd-log-line-trunc detect example" $ do
  results <- fmap MkResultText <$> (readIORef =<< runAndGetLogs args)
  V.verifyExpected results expected
  where
    args =
      withNoConfig
        [ "-l",
          "--cmd-log-line-trunc",
          "detect",
          "echo 'some ridiculously long command i mean is this really necessary' && sleep 5"
        ]
    -- NOTE: This test is based on the terminal size, which obviously varies
    -- per machine. Thus we only test a really low limit so that CI is likely
    -- to pass.
    expected =
      [ "[Command][echo 'some ridiculously"
      ]

withBaseArgs :: [String] -> [String]
withBaseArgs as =
  [ "-c",
    configPath
  ]
    <> as

withNoConfig :: [String] -> [String]
withNoConfig as =
  [ "--no-config"
  ]
    <> as

configPath :: String
configPath = "./examples/config.toml"
