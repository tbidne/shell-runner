-- | Provides TH for CLI args.
--
-- @since 0.5
module Shrun.Configuration.Args.TH
  ( getDefaultConfigTH,
  )
where

import Data.Text qualified as T
import Language.Haskell.TH (Code, Q)
import Language.Haskell.TH.Syntax (Lift (..), bindCode, runIO)
import Shrun.Prelude

-- | Reads examples/default.toml at compile time.
--
-- @since 0.5
getDefaultConfigTH :: Code Q (List Text)
getDefaultConfigTH = bindCode (runIO getDefault) liftTyped
  where
    getDefault = T.lines <$> readFileUtf8Lenient "examples/default.toml"