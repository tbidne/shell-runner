{-# LANGUAGE QuasiQuotes #-}

-- | Runs functional tests.
module Main (main) where

import Effectful.FileSystem.PathReader.Static qualified as PR
import Effectful.FileSystem.PathWriter.Static qualified as PW
import Effectful.Terminal.Static qualified as Term
import Functional.Buffering qualified as Buffering
import Functional.Examples qualified as Examples
import Functional.Miscellaneous qualified as Miscellaneous
import Functional.Notify qualified as Notify
import Functional.Prelude
import Functional.ReadStrategyTest (ReadStrategyOpt)
import Functional.TestArgs
  ( TestArgs
      ( MkTestArgs,
        configPath,
        rootDir,
        tmpDir
      ),
  )
import GHC.Conc.Sync (setUncaughtExceptionHandler)
import System.Environment.Guard (guardOrElse')
import System.Environment.Guard.Lifted (ExpectEnv (ExpectEnvSet))
import System.IO qualified as IO
import Test.Tasty qualified as Tasty
import Test.Tasty.Options (OptionDescription (Option))

-- | Entry point for functional tests.
main :: IO ()
main = do
  setUncaughtExceptionHandler (IO.putStrLn . displayException)
  Tasty.defaultMainWithIngredients ingredients $ Tasty.withResource setup teardown specs
  where
    ingredients =
      Tasty.includingOptions [Option @ReadStrategyOpt Proxy]
        : Tasty.defaultIngredients

specs :: IO TestArgs -> TestTree
specs args = do
  testGroup
    "Functional Tests"
    [ Examples.specs args,
      Buffering.specs,
      Miscellaneous.specs args,
      Notify.specs
    ]

setup :: IO TestArgs
setup = do
  rootTmpDir <- runFuncEff $ (</> [osp|shrun|]) <$> PR.getTemporaryDirectory
  let workingTmpDir = rootTmpDir </> tmpName

  -- Make sure we delete any leftover files from a previous run, so tests
  -- have a clean environment.
  runFuncEff $ PW.removeDirectoryRecursiveIfExists_ workingTmpDir

  cwd <- runFuncEff $ (</> tmpName) <$> PR.getCurrentDirectory
  let lp = cwd </> [osp|config.toml|]

  runFuncEff $ PW.createDirectoryIfMissing True workingTmpDir
  pure
    $ MkTestArgs
      { rootDir = rootTmpDir,
        tmpDir = workingTmpDir,
        configPath = lp
      }
  where
    tmpName = [osp|test|] </> [osp|functional|]

teardown :: TestArgs -> IO ()
teardown testArgs = guardOrElse' "NO_CLEANUP" ExpectEnvSet doNothing cleanup
  where
    cleanup = runFuncEff $ do
      let cwd = testArgs ^. #tmpDir

      -- see NOTE: [Test cleanup]
      PW.removeDirectoryRecursiveIfExists_ cwd

    doNothing =
      runFuncEff
        $ Term.putStrLn
        $ "*** Not cleaning up tmp dir: '"
        <> decodeLenient (testArgs ^. #tmpDir)
        <> "'"
