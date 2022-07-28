{-# LANGUAGE TemplateHaskell #-}

module Integration.Utils
  ( ConfigIO (..),
    _MkConfigIO,
    NoConfigIO (..),
    _MkNoConfigIO,
    makeEnvAndVerify,
  )
where

import Data.Maybe (isJust)
import Integration.Prelude as X
import Shrun.Configuration.Env (withEnv)
import Shrun.Configuration.Env.Types
  ( CmdDisplay,
    CmdLogging,
    StripControl,
    TruncRegion (..),
    Truncation,
  )
import Shrun.Data.Command (Command)
import Shrun.Data.NonEmptySeq (NonEmptySeq)
import Shrun.Data.Timeout (Timeout)
import Shrun.Effects.FileSystemReader (FileSystemReader (..))
import Shrun.Effects.FileSystemWriter (FileSystemWriter (..))
import Shrun.Effects.Mutable (Mutable (..))
import Shrun.Effects.Terminal (Terminal (..))
import System.Environment (withArgs)

-- IO that has a default config file specified at test/unit/Unit/toml/config.toml
newtype ConfigIO a = MkConfigIO (IO a)
  deriving
    ( Applicative,
      FileSystemWriter,
      Functor,
      Monad,
      MonadIO,
      MonadUnliftIO,
      Mutable,
      Terminal
    )
    via IO

makePrisms ''ConfigIO

instance FileSystemReader ConfigIO where
  getXdgConfig _ = pure "test/integration/toml"
  readFile = liftIO . readFile
  getFileSize = liftIO . getFileSize
  doesFileExist = liftIO . doesFileExist
  getArgs = liftIO getArgs

-- IO with no default config file
newtype NoConfigIO a = MkNoConfigIO (IO a)
  deriving
    ( Applicative,
      FileSystemWriter,
      Functor,
      Monad,
      MonadIO,
      MonadUnliftIO,
      Mutable,
      Terminal
    )
    via IO

makePrisms ''NoConfigIO

instance FileSystemReader NoConfigIO where
  getXdgConfig _ = pure "./"
  readFile = liftIO . readFile
  getFileSize = liftIO . getFileSize
  doesFileExist = liftIO . doesFileExist
  getArgs = liftIO getArgs

-- | Makes an 'Env' for the given monad and compares the result with the
-- expected params.
makeEnvAndVerify ::
  forall m.
  ( FileSystemReader m,
    FileSystemWriter m,
    MonadUnliftIO m,
    Mutable m,
    Terminal m
  ) =>
  -- | List of CLI arguments.
  List String ->
  -- | Natural transformation from m to IO.
  (forall x. m x -> IO x) ->
  Maybe Timeout ->
  Maybe () ->
  StripControl ->
  CmdLogging ->
  CmdDisplay ->
  Maybe (Truncation 'TCmdName) ->
  Maybe (Truncation 'TCmdLine) ->
  StripControl ->
  Bool ->
  NonEmptySeq Command ->
  Assertion
makeEnvAndVerify
  args
  toIO
  timeout
  fileLogging
  fileLogStripControl
  cmdLogging
  cmdDisplay
  cmdNameTrunc
  cmdLineTrunc
  stripControl
  disableLogging
  commands = do
    result <- toIO $ withRunInIO $ \runner ->
      withArgs args (runner (withEnv pure))

    timeout @=? result ^. #timeout
    fileLogStripControl @=? result ^. #fileLogStripControl
    cmdLogging @=? result ^. #cmdLogging
    cmdDisplay @=? result ^. #cmdDisplay
    cmdNameTrunc @=? result ^. #cmdNameTrunc
    stripControl @=? result ^. #stripControl
    disableLogging @=? result ^. #disableLogging
    commands @=? result ^. #commands

    let resultFileLogging = result ^. #fileLogging
    case (fileLogging, resultFileLogging) of
      (Just _, Just _) -> pure ()
      (Nothing, Nothing) -> pure ()
      (Just _, Nothing) -> assertFailure "Expected file logging to be enabled"
      (Nothing, Just _) -> assertFailure "Expected file logging to be disabled"

    -- Because the 'detect' option will read variable widths, depending on
    -- the terminal size. We use 'Just 0' as a sentinel for "do not test the
    -- actual value".
    let resultcmdLineTrunc = result ^. #cmdLineTrunc
    case cmdLineTrunc of
      Just 0 ->
        assertBool
          ("Should be just " <> show resultcmdLineTrunc)
          (isJust $ result ^. #cmdLineTrunc)
      other -> other @=? resultcmdLineTrunc
