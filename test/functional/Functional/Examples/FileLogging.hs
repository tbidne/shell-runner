{-# LANGUAGE QuasiQuotes #-}

module Functional.Examples.FileLogging (tests) where

import Effectful.FileSystem.PathReader.Static qualified as PR
import FileSystem.OsPath (unsafeEncode)
import Functional.Prelude
import Functional.TestArgs (TestArgs)
import Test.Shrun.Verifier qualified as V

-- NOTE: If tests in this module fail, fix then update configuration.md!

tests :: IO TestArgs -> TestTree
tests args =
  testGroup
    "FileLogging"
    ( [ fileLog args,
        fileLogDeleteOnSuccessFail args
      ]
        ++ multiTestReadStrategy testsParams
    )
  where
    testsParams :: List ReadStrategyTestParams
    testsParams =
      [ fileLogCommandNameTruncN args,
        fileLogDeleteOnSuccess args,
        fileLogLineTruncN args,
        fileLogModeAppend args,
        fileLogModeRename args,
        fileLogModeWrite args,
        fileLogStripControlAll args,
        fileLogStripControlNone args,
        fileLogStripControlSmart args
      ]

fileLog :: IO TestArgs -> TestTree
fileLog testArgs = testCase "Runs file-log example" $ do
  outFile <- (</> [osp|readme-file-out.log|]) . view #tmpDir <$> testArgs
  let outFileStr = unsafeDecode outFile
      args =
        withNoConfig
          [ "--file-log",
            outFileStr,
            "sleep 2",
            "bad",
            "for i in 1 2 3; do echo hi; sleep 1; done"
          ]

  resultsConsole <- runExitFailure args
  V.verifyExpected resultsConsole expectedConsole

  resultsFile <- readLogFile outFile
  V.verifyExpected resultsFile expectedFile
  where
    expectedConsole =
      [ withErrorPrefix "bad",
        withSuccessPrefix "sleep 2",
        withSuccessPrefix "for i in 1 2 3; do echo hi; sleep 1; done",
        finishedPrefix
      ]
    expectedFile =
      expectedConsole
        ++ [ withCommandPrefix "for i in 1 2 3; do echo hi; sleep 1; done" "hi"
           ]

fileLogCommandNameTruncN :: IO TestArgs -> ReadStrategyTestParams
fileLogCommandNameTruncN testArgs =
  ReadStrategyTestParametricSetup
    "Runs --file-log-command-name-trunc 10 example"
    run
    ( \_ -> do
        outFile <- (</> [osp|readme-file-log-command-name-trunc-out.log|]) . view #tmpDir <$> testArgs
        let outFileStr = unsafeDecode outFile
            args =
              withNoConfig
                [ "--file-log",
                  outFileStr,
                  "--file-log-command-name-trunc",
                  "10",
                  "for i in 1 2 3; do echo hi; sleep 1; done"
                ]

        pure (args, outFile)
    )
    ( \(resultsConsole, outFile) -> do
        V.verifyExpected resultsConsole expectedConsole

        resultsFile <- readLogFile outFile
        V.verifyExpected resultsFile expectedFile
    )
  where
    expectedConsole =
      [ withSuccessPrefix "for i in 1 2 3; do echo hi; sleep 1; done", -- not truncated
        withFinishedPrefix "3 seconds"
      ]
    expectedFile =
      [ withCommandPrefix "for i i..." "hi",
        withFinishedPrefix "3 seconds"
      ]

fileLogDeleteOnSuccess :: IO TestArgs -> ReadStrategyTestParams
fileLogDeleteOnSuccess testArgs =
  ReadStrategyTestParametricSetup
    "Runs file-log-delete-on-success example"
    run
    ( \_ -> do
        outFile <- (</> [osp|del-on-success.log|]) . view #tmpDir <$> testArgs
        let outFileStr = unsafeDecode outFile
            args =
              withNoConfig
                [ "--file-log",
                  outFileStr,
                  "--file-log-delete-on-success",
                  "sleep 2"
                ]
        pure (args, outFile)
    )
    ( \(resultsConsole, outFile) -> do
        V.verifyExpected resultsConsole expectedConsole

        exists <- runFuncEff $ PR.doesFileExist outFile

        assertBool "File should not exist" (not exists)
    )
  where
    expectedConsole =
      [ withSuccessPrefix "sleep 2",
        finishedPrefix
      ]

fileLogDeleteOnSuccessFail :: IO TestArgs -> TestTree
fileLogDeleteOnSuccessFail testArgs = testCase "Runs file-log-delete-on-success failure example" $ do
  outFile <- (</> [osp|del-on-success-fail.log|]) . view #tmpDir <$> testArgs
  let outFileStr = unsafeDecode outFile
      args =
        withNoConfig
          [ "--file-log",
            outFileStr,
            "--file-log-delete-on-success",
            "bad",
            "sleep 2"
          ]

  resultsConsole <- runExitFailure args
  V.verifyExpected resultsConsole expectedConsole

  exists <- runFuncEff $ PR.doesFileExist outFile

  assertBool "File should exist" exists

  resultsFile <- readLogFile outFile
  V.verifyExpected resultsFile expectedFile
  where
    expectedConsole =
      [ withErrorPrefix "bad",
        withSuccessPrefix "sleep 2",
        finishedPrefix
      ]
    expectedFile = expectedConsole

fileLogLineTruncN :: IO TestArgs -> ReadStrategyTestParams
fileLogLineTruncN testArgs =
  ReadStrategyTestParametricSetup
    "Runs --file-log-line-trunc 120 example"
    run
    ( \_ -> do
        outFile <- (</> [osp|line-trunc.log|]) . view #tmpDir <$> testArgs
        let outFileStr = unsafeDecode outFile
            args =
              withNoConfig
                [ "--file-log",
                  outFileStr,
                  "--file-log-line-trunc",
                  "120",
                  "echo 'some ridiculously long command i mean is this really necessary' && sleep 2"
                ]

        -- NOTE: We choose 120 so that we get _some_ chars rather than minimal ...,
        -- so the test is more precise.
        pure (args, outFile)
    )
    ( \(_, outFile) -> do
        resultsFile <- readLogFile outFile
        V.verifyExpected resultsFile expectedFile
    )
  where
    expectedFile =
      [ withCommandPrefix "echo 'some ridiculously long command i mean is this really necessary' && sleep 2" "Star...",
        withCommandPrefix "echo 'some ridiculously long command i mean is this really necessary' && sleep 2" "some..."
      ]

-- NOTE: File log mode tests are not configuration.md examples due to
-- simplicity.

-- NOTE: [File Log Mode tests]
--
-- The file log mode tests test the different behaviors of the log file:
--
-- append: logs are appended to the same file.
-- rename: we create multiple log files with sequential names.
-- write: the log file is overwritten.
--
-- In general, this means we create a base log file like
--
--     /tmp/test-name_read-strategy.log
--
-- where read-strategy is used to provide a unique path for each test
-- (e.g. we do not want fileLogModeAppend to use the same path for its
-- buffer and block tests, since the interference will cause failures).
--
-- We then check for existence and content, and then -- depending on the
-- test -- run checks against renamed log files i.e.
--
--     /tmp/test-name_read-strategy (1).log
---
-- For example, in the fileLogModeRename block test, we expect the following to
-- all exist:
--
--     /tmp/fileLogModeRename_block.log
--     /tmp/fileLogModeRename_block (1).log
--     /tmp/fileLogModeRename_block (2).log
--
-- OTOH, for fileLogModeAppend, we expect only
--
--     /tmp/fileLogModeAppend_block.log
--
-- to exist, while the others should __not__ exist. We use the mkLogPath
-- function to make creating these paths a little nicer.

fileLogModeAppend :: IO TestArgs -> ReadStrategyTestParams
fileLogModeAppend testArgs =
  ReadStrategyTestParametricSetup
    "Runs file-log-mode append"
    ( \xs -> do
        run xs
        run xs
        run xs
    )
    ( \rsType -> do
        tmpDir <- view #tmpDir <$> testArgs
        let outFile = mkLogPath tmpDir baseName rsType Nothing
            outFileStr = unsafeDecode outFile
            args =
              withNoConfig
                [ "--file-log",
                  outFileStr,
                  "--file-log-mode",
                  "append",
                  "sleep 2"
                ]
        pure (args, (tmpDir, rsType))
    )
    ( \(resultsConsole, (tmpDir, rsType)) -> do
        V.verifyExpected resultsConsole expectedConsole

        let log = mkLogPath tmpDir baseName rsType Nothing

        exists <- runFuncEff $ PR.doesFileExist log
        assertBool ("File should exist: " <> decodeLenient log) exists
        resultsFile <- readLogFile log
        V.verifyExpected resultsFile expectedConsole

        -- because we are appending 3 lines to the file 3 times
        9 @=? length resultsFile

        let log2 = mkLogPath tmpDir baseName rsType (Just 1)
            log3 = mkLogPath tmpDir baseName rsType (Just 2)

        for_ [log2, log3] $ \badLog -> do
          badExists <- runFuncEff $ PR.doesFileExist badLog
          assertBool ("File should not exist: " <> decodeLenient badLog) (not badExists)
    )
  where
    baseName = [osp|fileLogModeAppend_|]
    expectedConsole =
      [ withSuccessPrefix "sleep 2",
        finishedPrefix
      ]

fileLogModeRename :: IO TestArgs -> ReadStrategyTestParams
fileLogModeRename testArgs =
  ReadStrategyTestParametricSetup
    "Runs file-log-mode rename"
    ( \xs -> do
        run xs
        run xs
        run xs
    )
    ( \rsType -> do
        tmpDir <- view #tmpDir <$> testArgs
        let outFile = mkLogPath tmpDir baseName rsType Nothing
            outFileStr = unsafeDecode outFile
            args =
              withNoConfig
                [ "--file-log",
                  outFileStr,
                  "--file-log-mode",
                  "rename",
                  "sleep 2"
                ]
        pure (args, (tmpDir, rsType))
    )
    ( \(resultsConsole, (tmpDir, rsType)) -> do
        V.verifyExpected resultsConsole expectedConsole

        let log1 = mkLogPath tmpDir baseName rsType Nothing
            log2 = mkLogPath tmpDir baseName rsType (Just 1)
            log3 = mkLogPath tmpDir baseName rsType (Just 2)

        for_ [log1, log2, log3] $ \log -> do
          exists <- runFuncEff $ PR.doesFileExist log
          assertBool ("File should exist: " <> decodeLenient log) exists

          resultsFile <- readLogFile log
          V.verifyExpected resultsFile expectedFile

          3 @=? length resultsFile

        let badLog = mkLogPath tmpDir baseName rsType (Just 3)
        exists <- runFuncEff $ PR.doesFileExist badLog
        assertBool ("File should not exist: " <> decodeLenient badLog) (not exists)
    )
  where
    baseName = [osp|fileLogModeRename_|]
    expectedConsole =
      [ withSuccessPrefix "sleep 2",
        finishedPrefix
      ]
    expectedFile = expectedConsole

fileLogModeWrite :: IO TestArgs -> ReadStrategyTestParams
fileLogModeWrite testArgs =
  ReadStrategyTestParametricSetup
    "Runs file-log-mode write"
    ( \xs -> do
        run xs
        run xs
        run xs
    )
    ( \rsType -> do
        tmpDir <- view #tmpDir <$> testArgs
        let outFile = mkLogPath tmpDir baseName rsType Nothing
            outFileStr = unsafeDecode outFile
            args =
              withNoConfig
                [ "--file-log",
                  outFileStr,
                  "--file-log-mode",
                  "write",
                  "sleep 2"
                ]
        pure (args, (tmpDir, rsType))
    )
    ( \(resultsConsole, (tmpDir, rsType)) -> do
        V.verifyExpected resultsConsole expectedConsole

        let log = mkLogPath tmpDir baseName rsType Nothing

        exists <- runFuncEff $ PR.doesFileExist log
        assertBool ("File should exist: " <> decodeLenient log) exists
        resultsFile <- readLogFile log
        V.verifyExpected resultsFile expectedFile

        3 @=? length resultsFile

        let log2 = mkLogPath tmpDir baseName rsType (Just 1)
            log3 = mkLogPath tmpDir baseName rsType (Just 2)

        for_ [log2, log3] $ \badLog -> do
          badExists <- runFuncEff $ PR.doesFileExist badLog
          assertBool ("File should not exist: " <> decodeLenient badLog) (not badExists)
    )
  where
    baseName = [osp|fileLogModeWrite_|]
    expectedConsole =
      [ withSuccessPrefix "sleep 2",
        finishedPrefix
      ]
    expectedFile = expectedConsole

fileLogStripControlAll :: IO TestArgs -> ReadStrategyTestParams
fileLogStripControlAll testArgs =
  ReadStrategyTestParametricSetup
    "Runs file-log strip-control all example"
    run
    ( \_ -> do
        outFile <- (</> [osp|readme-file-out-strip-control-all.log|]) . view #tmpDir <$> testArgs
        let outFileStr = unsafeDecode outFile
            args =
              withNoConfig
                [ "--file-log",
                  outFileStr,
                  "--file-log-strip-control",
                  "all",
                  "printf ' foo \ESC[35m hello \ESC[3D bye '; sleep 2"
                ]
        pure (args, outFile)
    )
    ( \(_, outFile) -> do
        resultsFile <- readLogFile outFile
        V.verifyExpected resultsFile expectedFile
    )
  where
    expectedFile =
      [ withCommandPrefix "printf ' foo  hello  bye '; sleep 2" " foo  hello  bye "
      ]

fileLogStripControlNone :: IO TestArgs -> ReadStrategyTestParams
fileLogStripControlNone testArgs =
  ReadStrategyTestParametricSetup
    "Runs file-log strip-control none example"
    run
    ( \_ -> do
        outFile <- (</> [osp|readme-file-out-strip-control-none.log|]) . view #tmpDir <$> testArgs
        let outFileStr = unsafeDecode outFile
            args =
              withNoConfig
                [ "--file-log",
                  outFileStr,
                  "--file-log-strip-control",
                  "none",
                  "printf ' foo \ESC[35m hello \ESC[3D bye '; sleep 2"
                ]
        pure (args, outFile)
    )
    ( \(_, outFile) -> do
        resultsFile <- readLogFile outFile
        V.verifyExpected resultsFile expectedFile
    )
  where
    expectedFile =
      [ withCommandPrefix
          "printf ' foo  hello  bye '; sleep 2"
          " foo \ESC[35m hello \ESC[3D bye"
      ]

fileLogStripControlSmart :: IO TestArgs -> ReadStrategyTestParams
fileLogStripControlSmart testArgs =
  ReadStrategyTestParametricSetup
    "Runs file-log strip-control smart example"
    run
    ( \_ -> do
        outFile <- (</> [osp|readme-file-out-strip-control-smart.log|]) . view #tmpDir <$> testArgs
        let outFileStr = unsafeDecode outFile
            args =
              withNoConfig
                [ "--file-log",
                  outFileStr,
                  "--file-log-strip-control",
                  "smart",
                  "printf ' foo \ESC[35m hello \ESC[3D bye '; sleep 2"
                ]
        pure (args, outFile)
    )
    ( \(_, outFile) -> do
        resultsFile <- readLogFile outFile
        V.verifyExpected resultsFile expectedFile
    )
  where
    expectedFile =
      [ withCommandPrefix
          "printf ' foo  hello  bye '; sleep 2"
          " foo \ESC[35m hello  bye "
      ]

-- | mkLogPath is used to make log paths, based on the current read-strategy
-- (used for uniqueness) and sequential number (for file-log-mode rename)
--
-- Examples:
--
-- @
--   -- mkLogPath /tmp fileLogModeAppend_ block Nothing
--   "\/tmp\/fileLogModeAppend_block.log"
--
--   -- mkLogPath /tmp fileLogModeAppend_ block (Just 1)
--   "\/tmp\/fileLogModeAppend_block (1).log"
--
--   -- mkLogPath /tmp fileLogModeAppend_ block (Just 2)
--   "\/tmp\/fileLogModeAppend_block (2).log"
-- @
--
-- See NOTE: [File Log Mode tests] for the motivation.
mkLogPath ::
  -- | Temp directory
  OsPath ->
  -- | base name
  OsPath ->
  -- | read-strategy type
  OsPath ->
  -- | Sequence file number, if applicable
  Maybe Int ->
  -- | Combined name
  OsPath
mkLogPath tmpDir base rsType mSEqNum =
  tmpDir </> base <> rsType <> suffix
  where
    suffix = case mSEqNum of
      Nothing -> [osp|.log|]
      Just i -> [osp| (|] <> unsafeEncode (show i) <> [osp|).log|]
