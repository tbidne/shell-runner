-- | Provides types for the legend functionality.
--
-- @since 0.1
module ShellRun.Legend
  ( -- * Types
    LegendErr (..),
    LegendMap,

    -- * Parsing
    legendPathToMap,
  )
where

import Data.Text qualified as T
import ShellRun.Legend.Internal (LegendErr (..), LegendMap)
import ShellRun.Legend.Internal qualified as Internal
import ShellRun.Prelude

-- | Given a filepath, attempts to parse the file's contents into
-- a 'LegendMap'. If the file does not exist or the parsing fails
-- (see 'Internal.linesToMap'), an error will be returned.
--
-- @since 0.1
legendPathToMap :: FilePath -> IO (Either LegendErr LegendMap)
legendPathToMap legendPath = do
  res <- try (readFileUtf8Lenient legendPath) :: IO (Either SomeException Text)
  pure $ case res of
    Left err ->
      Left $
        FileErr $
          "Error reading legend file `"
            <> T.pack legendPath
            <> "`: "
            <> showt err
    Right contents -> Internal.linesToMap $ T.lines contents
{-# INLINEABLE legendPathToMap #-}
