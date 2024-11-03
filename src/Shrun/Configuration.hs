module Shrun.Configuration
  ( mergeConfig,
  )
where

import Shrun.Configuration.Args (Args)
import Shrun.Configuration.Data.Core (mergeCoreConfig)
import Shrun.Configuration.Data.MergedConfig
  ( MergedConfig
      ( MkMergedConfig,
        commands,
        coreConfig
      ),
  )
import Shrun.Configuration.Legend qualified as Legend
import Shrun.Configuration.Toml (Toml)
import Shrun.Data.Command (CommandP (MkCommandP))
import Shrun.Prelude

-- | Merges Args and Toml together, filling in necessary defaults and
-- doing some light processing.
--
-- We want this function to do as much to prepare the final config as
-- possible. For instance, in addition to filling in defaults, we also process
-- commands via the legend (MonadThrow) and detect the terminal width for
-- command logging's lineTrunc field (Terminal).
--
-- This is very nearly pure, except for the aforementioned effects.
-- The only remaining tasks the Env needs to take care of is IO that we
-- really can't test anyway, such as opening file handles and creating
-- queues.
mergeConfig ::
  ( HasCallStack,
    Terminal :> es
  ) =>
  Args ->
  Maybe Toml ->
  Eff es MergedConfig
mergeConfig args mToml = do
  case mToml of
    Nothing -> do
      let commands = MkCommandP Nothing <$> cmdsText

      coreConfig <-
        mergeCoreConfig
          commands
          (args ^. #coreConfig)
          Nothing

      pure
        $ MkMergedConfig
          { coreConfig,
            commands
          }
    (Just toml) -> do
      commands <- case toml ^. #legend of
        Nothing -> pure $ MkCommandP Nothing <$> cmdsText
        Just aliases -> case Legend.linesToMap aliases of
          Right mp -> case Legend.translateCommands mp cmdsText of
            Right cmds -> pure cmds
            Left err -> throwM err
          Left err -> throwM err

      coreConfig <-
        mergeCoreConfig
          commands
          (args ^. #coreConfig)
          (Just $ toml ^. #coreConfig)

      pure
        $ MkMergedConfig
          { coreConfig,
            commands
          }
  where
    cmdsText = args ^. #commands
