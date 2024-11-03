{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

module Integration.Utils
  ( -- * Running
    runConfigIO,
    runNoConfigIO,

    -- * Assertions
    makeConfigAndAssertEq,
    makeConfigAndAssertFieldEq,
    CompareField (..),
    (^=@),
    (^?=@),

    -- * Misc
    defaultConfig,
    notifySystemOSDBus,
    notifySystemOSNotifySend,
  )
where

import DBus.Client
  ( Client
      ( Client,
        clientInterfaces,
        clientObjects,
        clientPendingCalls,
        clientSignalHandlers,
        clientSocket,
        clientThreadID
      ),
  )
import Data.Sequence.NonEmpty qualified as NESeq
import Data.Text qualified as T
import Effectful.FileSystem.PathReader.Dynamic
  ( PathReader
      ( DoesDirectoryExist,
        DoesFileExist,
        GetFileSize,
        GetXdgDirectory
      ),
  )
import Effectful.Terminal.Dynamic
  ( Terminal (GetTerminalSize, PutStrLn),
    Window (Window),
  )
import Integration.Prelude as X
import Shrun.Configuration.Data.MergedConfig (MergedConfig, defaultMergedConfig)
import Shrun.Configuration.Data.Notify.System (NotifySystemMerged)
import Shrun.Configuration.Data.Notify.System qualified as Notify.System
import Shrun.Configuration.Env qualified as Env
import Shrun.Notify.DBus (DBus (ConnectSession, Notify))

{- ORMOLU_DISABLE -}

-- IO that has a default config file specified at test/unit/Unit/toml/config.toml
runPathReaderConfig :: (IOE :> es) => Eff (PathReader : es) a -> Eff es a
runPathReaderConfig = reinterpret runPathReader $ \_ -> \case
  GetFileSize p -> getFileSize p
  DoesFileExist p -> doesFileExist p
  DoesDirectoryExist p -> doesDirectoryExist p
#if OSX
  GetXdgDirectory _ _ -> pure [ospPathSep|test/integration/toml/osx|]
#else
  GetXdgDirectory _ _ -> pure [ospPathSep|test/integration/toml|]
#endif
  _ -> unimplWith "runPathReaderConfig"

{- ORMOLU_ENABLE -}

runTerminalConfig ::
  ( IORefE :> es,
    Reader (IORef [Text]) :> es
  ) =>
  Eff (Terminal : es) a ->
  Eff es a
runTerminalConfig = interpret_ $ \case
  PutStrLn t -> do
    logsRef <- ask
    modifyIORef' logsRef (T.pack t :)
  GetTerminalSize -> pure (Window 23 87)
  _ -> unimplWith "runTerminalConfig"

runDBusConfig :: Eff (DBus : es) a -> Eff es a
runDBusConfig = interpret_ $ \case
  ConnectSession ->
    pure
      $ Client
        { clientSocket = unimpl,
          clientPendingCalls = unimpl,
          clientSignalHandlers = unimpl,
          clientObjects = unimpl,
          clientThreadID = unimpl,
          clientInterfaces = unimpl
        }
  Notify _ _ -> unimpl

runConfigIO ::
  (MonadIO m) =>
  Eff
    [ DBus,
      FileReader,
      FileWriter,
      HandleWriter,
      Optparse,
      PathReader,
      PathWriter,
      Terminal,
      Environment,
      IORefE,
      Reader (IORef (List Text)),
      Concurrent,
      IOE
    ]
    a ->
  IORef [Text] ->
  m a
runConfigIO m ref =
  liftIO
    . runEff
    . runConcurrent
    . runReader ref
    . runIORef
    . runEnvironment
    . runTerminalConfig
    . runPathWriter
    . runPathReaderConfig
    . runOptparse
    . runHandleWriter
    . runFileWriter
    . runFileReader
    . runDBusConfig
    $ m

runNoConfigIO ::
  Eff
    [ DBus,
      FileReader,
      FileWriter,
      HandleWriter,
      Optparse,
      PathReader,
      PathWriter,
      Terminal,
      Environment,
      IORefE,
      Reader (IORef (List Text)),
      Concurrent,
      IOE
    ]
    a ->
  IORef [Text] ->
  IO a
runNoConfigIO m ref =
  runEff
    . runConcurrent
    . runReader ref
    . runIORef
    . runEnvironment
    . runTerminalConfig
    . runPathWriter
    . runPathReaderNoConfig
    . runOptparse
    . runHandleWriter
    . runFileWriter
    . runFileReader
    . runDBusConfig
    $ m

runPathReaderNoConfig :: (IOE :> es) => Eff (PathReader : es) a -> Eff es a
runPathReaderNoConfig = reinterpret runPathReader $ \_ -> \case
  DoesFileExist p -> doesFileExist p
  GetXdgDirectory _ _ -> pure [osp|./|]
  _ -> unimplWith "runPathReaderNoConfig"

-- | Makes a 'MergedConfig' for the given monad and compares the result with
-- the expectation.
makeConfigAndAssertEq ::
  forall es.
  ( Environment :> es,
    FileReader :> es,
    Optparse :> es,
    PathReader :> es,
    Terminal :> es
  ) =>
  -- | List of CLI arguments.
  List String ->
  -- | Natural transformation from m to IO.
  (forall x. Eff es x -> IO x) ->
  -- | Expectation.
  MergedConfig ->
  PropertyT IO ()
makeConfigAndAssertEq args toIO expected = do
  result <- makeMergedConfig args toIO
  expected === result

-- | Used for testing a selection of MergedConfig's fields rather than the
-- entire structure.
data CompareField where
  -- | Tests a lens.
  MkCompareField :: (Eq a, Show a) => Lens' MergedConfig a -> a -> CompareField
  -- | Tests an affine traversal.
  MkCompareFieldMaybe ::
    (Eq a, Show a) =>
    AffineTraversal' MergedConfig a ->
    Maybe a ->
    CompareField

-- | Alias for 'MkCompareField'.
(^=@) :: (Eq a, Show a) => Lens' MergedConfig a -> a -> CompareField
l ^=@ r = MkCompareField l r

infix 1 ^=@

-- | Alias for 'MkCompareFieldMaybe'.
(^?=@) :: (Eq a, Show a) => AffineTraversal' MergedConfig a -> Maybe a -> CompareField
l ^?=@ r = MkCompareFieldMaybe l r

infix 1 ^?=@

-- | Like 'makeConfigAndAssertEq' except we only compare select fields.
makeConfigAndAssertFieldEq ::
  forall es.
  ( Environment :> es,
    FileReader :> es,
    Optparse :> es,
    PathReader :> es,
    Terminal :> es
  ) =>
  -- | List of CLI arguments.
  List String ->
  -- | Natural transformation from m to IO.
  (forall x. Eff es x -> IO x) ->
  -- | List of expectations.
  List CompareField ->
  PropertyT IO ()
makeConfigAndAssertFieldEq args toIO comparisons = do
  result <- makeMergedConfig args toIO

  for_ comparisons $ \case
    MkCompareField l expected -> expected === result ^. l
    MkCompareFieldMaybe l expected -> expected === result ^? l

makeMergedConfig ::
  forall es.
  ( Environment :> es,
    FileReader :> es,
    Optparse :> es,
    PathReader :> es,
    Terminal :> es
  ) =>
  -- | List of CLI arguments.
  List String ->
  -- | Natural transformation from m to IO.
  (forall x. Eff es x -> IO x) ->
  PropertyT IO MergedConfig
makeMergedConfig args toIO = do
  result <- liftIO $ toIO $ withArgs args Env.getMergedConfig

  annotateShow args

  pure result

-- | Convenience for tests expecting a default config. The test should
-- pass a single command 'cmd'.
defaultConfig :: MergedConfig
defaultConfig = defaultMergedConfig $ NESeq.singleton "cmd"

notifySystemOSDBus :: NotifySystemMerged
#if OSX
notifySystemOSDBus = Notify.System.AppleScript
#else
notifySystemOSDBus = Notify.System.DBus ()
#endif

notifySystemOSNotifySend :: NotifySystemMerged
#if OSX
notifySystemOSNotifySend = Notify.System.AppleScript
#else
notifySystemOSNotifySend = Notify.System.NotifySend
#endif
