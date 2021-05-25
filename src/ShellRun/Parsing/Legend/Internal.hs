{-# LANGUAGE ImportQualifiedPost #-}

module ShellRun.Parsing.Legend.Internal
  ( linesToMap,
  )
where

import Control.Applicative qualified as A
import Control.Monad qualified as M
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import ShellRun.Types.Legend (LegendErr (..), LegendMap)

linesToMap :: [Text] -> Either LegendErr LegendMap
linesToMap = foldr f (Right Map.empty)
  where
    f "" mp = mp
    f (T.stripPrefix "#" -> Just _) mp = mp
    f line mp = M.join $ A.liftA2 insertPair (parseLine line) mp
    insertPair (key, cmd) mp =
      case Map.lookup key mp of
        Just _ -> Left $ DuplicateKeyErr key
        Nothing -> Right $ Map.insert key cmd mp

parseLine :: Text -> Either LegendErr (Text, Text)
parseLine l =
  case T.break (== '=') l of
    ("", _) -> Left $ EntryErr $ "Key cannot be empty: " <> l
    (_, "") -> Left $ EntryErr $ "Value cannot be empty: " <> l
    -- T.tail is safe because v can't be empty, or it would have matched
    -- the previous pattern.
    (k, v) -> Right (k, T.tail v)