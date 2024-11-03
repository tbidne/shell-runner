{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances #-}

module Integration.Prelude
  ( module X,
    TestArgs (..),
    getExampleConfig,
    getExampleConfigOS,
    getIntConfig,
    getIntConfigOS,
    concatDirs,
    runIntEff,
  )
where

import Effectful.FileSystem.PathReader.Static qualified as PR
import Effectful.FileSystem.PathWriter.Static qualified as PW
import Effectful.Terminal.Static qualified as Term
import FileSystem.OsPath as X
  ( combineFilePaths,
    unsafeDecode,
    unsafeEncode,
    (</>!),
  )
import Hedgehog as X
  ( Property,
    PropertyT,
    annotate,
    annotateShow,
    assert,
    property,
    withTests,
    (===),
  )
import Shrun.Prelude as X
import Test.Tasty as X
  ( TestName,
    TestTree,
    defaultMain,
    testGroup,
    withResource,
  )
import Test.Tasty.HUnit as X
  ( Assertion,
    assertBool,
    assertFailure,
    testCase,
    (@=?),
  )
import Test.Tasty.Hedgehog as X (testPropertyNamed)

data TestArgs = MkTestArgs
  { rootTmpDir :: OsPath,
    workingTmpDir :: OsPath
  }
  deriving stock (Eq, Show)

instance
  ( k ~ A_Lens,
    a ~ OsPath,
    b ~ OsPath
  ) =>
  LabelOptic "rootTmpDir" k TestArgs TestArgs a b
  where
  labelOptic =
    lensVL
      $ \f (MkTestArgs a1 a2) ->
        fmap
          (\b -> MkTestArgs b a2)
          (f a1)
  {-# INLINE labelOptic #-}

instance
  ( k ~ A_Lens,
    a ~ OsPath,
    b ~ OsPath
  ) =>
  LabelOptic "workingTmpDir" k TestArgs TestArgs a b
  where
  labelOptic =
    lensVL
      $ \f (MkTestArgs a1 a2) ->
        fmap
          (\b -> MkTestArgs a1 b)
          (f a2)
  {-# INLINE labelOptic #-}

-- | Retrieves file path from the examples directory, potentially appending
-- the os onto the filename (e.g. osx).
getExampleConfigOS :: FilePath -> FilePath
getExampleConfigOS fileName =
  concatDirs ["examples", osExt fileName]
    <> ".toml"

-- | Retrieves file path from the examples directory.
getExampleConfig :: FilePath -> FilePath
getExampleConfig fileName =
  concatDirs ["examples", fileName]
    <> ".toml"

-- | Retrieves file path from the integration directory, potentially appending
-- the os onto the filename (e.g. osx).
getIntConfigOS :: FilePath -> FilePath
getIntConfigOS fileName =
  concatDirs ["test", "integration", "toml", osExt fileName]
    <> ".toml"

-- | Retrieves file path from the integration directory.
getIntConfig :: FilePath -> FilePath
getIntConfig fileName =
  concatDirs ["test", "integration", "toml", fileName]
    <> ".toml"

concatDirs :: List FilePath -> FilePath
concatDirs = foldr combineFilePaths []

-- | General effects we use for test definition / setup. This should all be
-- static since should be no mocking.
runIntEff ::
  (HasCallStack, MonadIO m) =>
  Eff
    [ FileWriter,
      Term.Terminal,
      PW.PathWriter,
      PR.PathReader,
      IOE
    ]
    a ->
  m a
runIntEff =
  liftIO
    . runEff
    . PR.runPathReader
    . PW.runPathWriter
    . Term.runTerminal
    . runFileWriter

osExt :: FilePath -> FilePath
#if OSX
osExt = (<> "_osx")
#else
osExt = id
#endif
