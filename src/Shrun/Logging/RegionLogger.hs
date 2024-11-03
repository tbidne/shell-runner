{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Provides functionality for logging to a specific region
-- (i.e. for concurrent console logging).
module Shrun.Logging.RegionLogger
  ( -- * Effect
    RegionLogger (..),
    logGlobal,
    logRegion,
    withRegion,
    displayRegions,

    -- * Handler
    runRegionLogger,
  )
where

import Shrun.Logging.Types.Internal
  ( LogMode
      ( LogModeAppend,
        LogModeFinish,
        LogModeSet
      ),
  )
import Shrun.Prelude
import System.Console.Regions qualified as Regions

type RegionLogger :: Type -> Effect
data RegionLogger r :: Effect where
  LogGlobal :: forall r m. Text -> RegionLogger r m ()
  LogRegion :: forall r m. LogMode -> r -> Text -> RegionLogger r m ()
  WithRegion :: forall r m a. RegionLayout -> (r -> m a) -> RegionLogger r m a
  DisplayRegions :: forall r m a. m a -> RegionLogger r m a

-- | @since 0.1
type instance DispatchOf (RegionLogger _) = Dynamic

runRegionLogger ::
  ( r ~ ConsoleRegion,
    HasCallStack,
    IOE :> es,
    Terminal :> es
  ) =>
  Eff (RegionLogger r : es) a ->
  Eff es a
runRegionLogger = interpret $ \env -> \case
  LogGlobal t -> putTextLn t
  LogRegion m r t -> case m of
    LogModeSet -> liftIO $ Regions.setConsoleRegion r t
    LogModeAppend -> liftIO $ Regions.appendConsoleRegion r t
    LogModeFinish -> liftIO $ Regions.finishConsoleRegion r t
  WithRegion layout onRegion -> localSeqUnliftIO env $ \runInIO ->
    liftIO $ Regions.withConsoleRegion layout (runInIO . onRegion)
  DisplayRegions m ->
    localSeqUnliftIO env $ \runInIO ->
      liftIO $ Regions.displayConsoleRegions (runInIO m)

logGlobal ::
  forall r es.
  ( HasCallStack,
    RegionLogger r :> es
  ) =>
  Text ->
  Eff es ()
logGlobal = send . LogGlobal @r

logRegion ::
  forall r es.
  ( HasCallStack,
    RegionLogger r :> es
  ) =>
  LogMode ->
  r ->
  Text ->
  Eff es ()
logRegion m r = send . LogRegion @r m r

withRegion ::
  forall r es a.
  ( HasCallStack,
    RegionLogger r :> es
  ) =>
  RegionLayout ->
  (r -> Eff es a) ->
  Eff es a
withRegion l = send . WithRegion @r l

displayRegions ::
  forall r es a.
  ( HasCallStack,
    RegionLogger r :> es
  ) =>
  Eff es a ->
  Eff es a
displayRegions = send . DisplayRegions @r
