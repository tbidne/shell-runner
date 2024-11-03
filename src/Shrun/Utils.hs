{-# LANGUAGE ViewPatterns #-}

-- | Provides utilities.
module Shrun.Utils
  ( -- * Text Utils
    breakStripPoint,
    truncateIfNeeded,
    stripControlAll,
    stripControlSmart,
    escapeDoubleQuotes,

    -- * Time Utils
    diffTime,
    timeSpecToRelTime,
    foldMap1,

    -- * Misc Utils
    fmtUnrecognizedError,
    parseByteText,
    whileM_,
    whenLeft,
    untilJust,
    unsafeListToNESeq,
    (∸),
    readStripUnderscores,
  )
where

import Data.Bytes (Conversion (convert), SomeSize, parse)
import Data.Char (isControl, isLetter)
import Data.Either (either)
import Data.Sequence.NonEmpty qualified as NESeq
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder (Builder)
import Data.Text.Lazy.Builder qualified as TLB
import Data.Time.Relative (RelativeTime, fromSeconds)
import Effectful.Time.Dynamic (TimeSpec, diffTimeSpec)
import GHC.Exts (IsList (fromList))
import Shrun.Data.Text (UnlinedText)
import Shrun.Data.Text qualified as ShrunText
import Shrun.Prelude
import Text.Read (Read)
import Text.Read qualified as TR

-- $setup
-- >>> :set -XOverloadedLists
-- >>> import Data.List.NonEmpty (NonEmpty (..))
-- >>> import Data.Semigroup (Sum (..))
-- >>> import Data.Text qualified as T
-- >>> import Effectful.Time.Dynamic (TimeSpec (..))
-- >>> import Shrun.Prelude

-- | For given \(x, y\), returns the absolute difference \(|x - y|\)
-- in seconds.
--
-- ==== __Examples__
-- >>> :{
--   let t1 = MkTimeSpec 5 0
--       -- 20 s + 1 billion ns = 21 s
--       t2 = MkTimeSpec 20 1_000_000_000
--   in diffTime t1 t2
-- :}
-- 16
diffTime :: TimeSpec -> TimeSpec -> Natural
diffTime t1 t2 = view #sec $ diffTimeSpec t1 t2

-- | Transforms a 'TimeSpec' into a 'RelativeTime'.
timeSpecToRelTime :: TimeSpec -> RelativeTime
timeSpecToRelTime = fromSeconds . view #sec

-- | Relaxes 'foldMap'\'s 'Monoid' constraint to 'Semigroup'. Requires a
-- starting value. This will have to do until semigroupoids' Foldable1 is
-- in base.
--
-- ==== __Examples__
-- >>> foldMap1 @List Sum 0 [1..4]
-- Sum {getSum = 10}
--
-- >>> -- Silly, but demonstrates usage i.e. with non-monoid NonEmpty.
-- >>> foldMap1 @List (:| []) 1 [2,3,4]
-- 1 :| [2,3,4]
foldMap1 :: (Foldable f, Semigroup s) => (a -> s) -> a -> f a -> s
foldMap1 f x xs = foldr (\b g y -> f y <> g b) f xs x

-- | Wrapper for 'Text'\'s 'T.breakOn' that differs in that:
--
-- 1. If the @needle@ is found within the @haystack@, we do not include it
-- in the second part of the pair.
--
-- ==== __Examples__
-- >>> -- Data.Text
-- >>> T.breakOn "=" "HEY=LISTEN"
-- ("HEY","=LISTEN")
--
-- >>> -- Shrun.Utils.Text
-- >>> breakStripPoint "=" "HEY=LISTEN"
-- ("HEY","LISTEN")
--
-- Other examples:
--
-- >>> breakStripPoint "=" "HEYLISTEN"
-- ("HEYLISTEN","")
--
-- >>> breakStripPoint "=" "=HEYLISTEN"
-- ("","HEYLISTEN")
--
-- >>> breakStripPoint "=" "HEYLISTEN="
-- ("HEYLISTEN","")
--
-- >>> breakStripPoint "=" "HEY==LISTEN"
-- ("HEY","=LISTEN")
breakStripPoint :: Text -> Text -> Tuple2 Text Text
breakStripPoint point txt = case T.breakOn point txt of
  (x, T.stripPrefix point -> Just y) -> (x, y)
  pair -> pair

-- | For 'Natural' \(n\) and 'Text' \(t = t_0 t_1 \ldots t_m\), truncates
-- \(t\) if \(m > n\). In this case, \(t\) is truncated to \(n - 3\), and an
-- ellipsis ( \(\ldots\) ) is appended. We are left with a string with
-- length exactly \(n\):
--
-- \[
-- t_0 t_1 \ldots t_{n-3} \text{...} \quad \text{-- 3 literal } `\text{.' chars appended}
-- \]
--
-- ==== __Examples__
-- >>> truncateIfNeeded 7 "hi"
-- "hi"
--
-- >>> truncateIfNeeded 10 "This is 21 chars long"
-- "This is..."
truncateIfNeeded :: Int -> Text -> Text
truncateIfNeeded n txt
  | T.length txt <= n = txt
  | otherwise = txt'
  where
    txt' = T.take (n - 3) txt <> "..."

-- NOTE: [StripControl Newlines]
--
-- Applying stripControl to text that has newlines in it can produce poorly
-- formatted text. This is due to newlines being stripped, so e.g. t1\nt2
-- becomes t1t2. Hence we require 'UnlinedText'.

-- | Strips all control chars, including ansi escape sequences.
--
-- ==== __Examples__
--
-- >>> stripControlAll "foo\ESC[0;3Abar \n baz"
-- "foobar  baz"
stripControlAll :: UnlinedText -> UnlinedText
stripControlAll =
  -- The ansi stripping must come first. For example, if we strip control
  -- chars from "\ESC[0;3mfoo" we get "0;3mfoo", and then stripAnsiAll will
  -- no longer recognize this as an ansi sequences - i.e. this will leave
  -- remnants from the ansi sequences.
  --
  -- By performing stripAnsiAll first, we remove entire ansi sequences,
  -- then remove other control chars (e.g. newlines, tabs).
  ShrunText.reallyUnsafeMap (T.filter (not . isControl) . stripAnsiAll)

-- | Strips control chars, including most ansi escape sequences. We leave
-- behind SGR ansi escape sequences e.g. text coloring. See
-- https://en.wikipedia.org/wiki/ANSI_escape_code#SGR_(Select_Graphic_Rendition)_parameters.
--
-- ==== __Examples__
--
-- >>> stripControlSmart "foo\ESC[0;3Abar \n baz"
-- "foobar  baz"
--
-- >>> stripControlSmart "foo\ESC[0;3mbar \n baz"
-- "foo\ESC[0;3mbar  baz"
stripControlSmart :: UnlinedText -> UnlinedText
stripControlSmart =
  -- Like 'stripControlAll', we need to handle the ansi sequences first.
  -- Because we actually leave some sequences behind, we need to be more
  -- surgical removing the rest of the control chars (e.g. newline, tabs).
  ShrunText.reallyUnsafeMap (T.filter ctrlToFilter . stripAnsiControl)
  where
    -- stripAnsiControl should be handling all \ESC sequences, so we should
    -- be safe to ignore these, accomplishing our goal of preserving the SGR
    -- sequences. If this is too aggressive, we can instead attempt to strip
    -- out the known 'bad' control chars e.g.
    --
    --   ctrlToFilter = not . (`elem` ['\n', '\t', '\v'])
    --
    ctrlToFilter c
      | isControl c = c == '\ESC'
      | otherwise = True

-- | Strips all ansi sequences from the given text.
--
-- ==== __Examples__
--
-- @
-- stripAnsiAll "foo\ESC[0;3Abar"
-- @
stripAnsiAll :: Text -> Text
stripAnsiAll = T.concat . fmap (view _1) . splitAnsi

-- | Strips ansi control sequences only.
--
-- ==== __Examples__
--
-- @
-- stripAnsiControl "foo\ESC[0;3Abar"
-- "foobar"
--
-- stripAnsiControl "foo\ESC[0;3mbar"
-- "foo\ESC[0;3mbar"
-- @
stripAnsiControl :: Text -> Text
stripAnsiControl txt =
  foldl' f "" splitTxt
  where
    splitTxt = splitAnsi txt
    f acc (preAnsi, code, withAnsi)
      | nonControlAnsi code = acc <> withAnsi
      | otherwise = acc <> preAnsi

nonControlAnsi :: Text -> Bool
nonControlAnsi ansi = case T.unsnoc ansi of
  -- 'm' equals color: only code we consider 'good' for now
  Just (_, 'm') -> True
  _ -> False

-- tuple is: (text, ansi_code, ansi_code <> text)
-- example: splitAnsi "foo\ESC[0;3mbar"
splitAnsi :: Text -> [(Text, Text, Text)]
splitAnsi "" = []
splitAnsi t =
  -- (foo, \ESC[0;3m, bar) : ...
  (preAnsi, ansiCode, preAnsi <> ansiCode) : rest
  where
    -- (foo, \ESC[0;3mbar)
    (!preAnsi, !withAnsiFull) = T.breakOn "\ESC[" t
    -- (\ESC[0;3, mbar)
    (!ansiCodeNoChar, !withAnsiChar) = T.break isLetter withAnsiFull
    -- (\ESC[0;3m, bar)
    (!ansiCode, !rest) = case T.uncons withAnsiChar of
      -- (m, bar)
      Just (!ansiChar, !rest') -> (T.snoc ansiCodeNoChar ansiChar, splitAnsi rest')
      Nothing -> (ansiCodeNoChar, [])

-- | Parses bytes with arbitrary units and converts to bytes. First attempts
-- to parse as a 'Natural' so we do not lose precision. If that fails, falls
-- back to 'Double'.
--
-- ==== __Examples__
--
-- >>> parseByteText "120 mb"
-- Right (MkBytes 120000000)
--
-- >>> parseByteText "4.5 terabytes"
-- Right (MkBytes 4500000000000)
parseByteText :: Text -> Either Text (Bytes B Natural)
parseByteText txt =
  case parse @(SomeSize Natural) txt of
    Right b -> Right $ convert (Proxy @B) b
    Left _ -> case parse @(SomeSize Double) txt of
      Right b -> Right (truncate <$> convert (Proxy @B) b)
      Left err -> Left err

-- | Runs the action when it is 'Left'.
whenLeft :: (Applicative f) => Either a b -> (a -> f ()) -> f ()
whenLeft e action = either action (const (pure ())) e

-- | @whileM_ mb ma@ executes @ma@ as long as @mb@ returns 'True'.
whileM_ :: (Monad m) => m Bool -> m a -> m ()
whileM_ mb ma = go
  where
    go =
      mb >>= \case
        True -> ma *> go
        False -> pure ()

-- | Executes the monadic action until we receive a 'Just', returning the
-- value.
untilJust :: (Monad m) => m (Maybe b) -> m b
untilJust m = go
  where
    go =
      m >>= \case
        Nothing -> go
        Just x -> pure x

{- HLINT ignore unsafeListToNESeq "Redundant bracket" -}

unsafeListToNESeq :: (HasCallStack) => List a -> NESeq a
unsafeListToNESeq [] = error "[Shrun.Utils]: empty list"
unsafeListToNESeq xs = NESeq.fromList $ fromList xs

-- | Escape double quotes in strings.
escapeDoubleQuotes :: Text -> Text
escapeDoubleQuotes = TL.toStrict . TLB.toLazyText . T.foldl' go ""
  where
    go :: Builder -> Char -> Builder
    go acc '"' = acc <> "\\\""
    go acc c = acc <> TLB.singleton c

-- | "monus" i.e. subtraction clamped to zero
(∸) :: (Ord a, Num a) => a -> a -> a
x ∸ y =
  if y > x
    then 0
    else x - y

infixl 6 ∸

readStripUnderscores :: (MonadFail m, Read a) => Text -> m a
readStripUnderscores t = case TR.readEither s of
  Left err -> fail $ "Could not read '" ++ s ++ "': " ++ err
  Right x -> pure x
  where
    noUnderscores = T.replace "_" "" t
    s = T.unpack noUnderscores

-- | Provides a standard format for "unrecognized param" failures.
fmtUnrecognizedError ::
  ( IsString a,
    Monoid a
  ) =>
  -- | Field name.
  a ->
  -- | Valid values.
  a ->
  -- | Bad unrecognized value or error message.
  a ->
  -- | Error message.
  a
fmtUnrecognizedError fieldName validVals badValue =
  mconcat
    [ "Error parsing ",
      fieldName,
      ": '",
      badValue,
      "'. Expected one of ",
      validVals,
      "."
    ]
