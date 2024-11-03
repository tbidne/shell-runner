{-# LANGUAGE UndecidableInstances #-}

module Shrun.Configuration.Data.CommandLogging.ReadSize
  ( ReadSize (..),
    parseReadSize,
  )
where

import Shrun.Configuration.Default (Default (def))
import Shrun.Prelude
import Shrun.Utils qualified as U

-- | Read size for command logs.
newtype ReadSize = MkReadSize {unReadSize :: Bytes B Int}
  deriving stock (Eq, Show)

instance
  ( k ~ An_Iso,
    a ~ Bytes B Int,
    b ~ Bytes B Int
  ) =>
  LabelOptic
    "unReadSize"
    k
    ReadSize
    ReadSize
    a
    b
  where
  labelOptic = iso (\(MkReadSize x) -> x) MkReadSize

instance Default ReadSize where
  -- NOTE: [Command log splitting]
  --
  -- (Streamed) command logs can be split based on the values of poll-interval
  -- and read-size. For instance, suppose we are running a command that
  -- prints a total of 3 logs, at a rate of 1 log per second:
  --
  -- "foo"
  -- "bar"
  -- "baz"
  --
  -- Further, suppose that poll-interval is the usual 10,000 (1/100 of a
  -- second), and that read-size is 5 b. The timeline for what happens looks
  -- like (HH-MM-SS):
  --
  -- [00:00:00.00] cmd prints "foo" -- process buffer has "foo"
  -- [00:00:00.01] shrun reads and prints "foo"
  -- [00:00:01.00] cmd prints "bar" -- process buffer has "bar"
  -- [00:00:01.01] shrun reads and prints "bar"
  -- [00:00:02.00] cmd prints "baz" -- process buffer has "baz"
  -- [00:00:02.01] shrun reads and prints "baz"
  --
  -- Now, suppose that poll-interval is 5 seconds. The breakdown is:
  --
  -- [00:00:00.00] cmd prints "foo" -- process buffer has "foo"
  -- [00:00:01.00] cmd prints "bar" -- process buffer has "foobar"
  -- [00:00:02.00] cmd prints "baz" -- process buffer has "foobarbaz"
  -- [00:00:05.00] shrun reads and prints "fooba"
  -- [00:00:10.00] shrun reads and prints "rbaz"
  --
  -- That is, because cmd was printing logs faster than shrun was reading them,
  -- the buffer was able to grow larger than the read-size, so logs were cut
  -- off. How can we avoid this? Several options:
  --
  -- 1. Make poll-interval faster so that we are less likely to accumulate
  --    multiple logs and thus break the read-size. The downside here is that
  --    the faster we make poll-interval, the higher the CPU usage. The current
  --    1/100 a second (poll-interval := 10,000) was chosen since the CPU usage
  ---   is low, and any faster starts to get high.
  --
  -- 2. Increase the read-size. Experimentally, building GHC with a '1 kb'
  --    limit still sees logs get split. '16 kb', on the other hand, didn't
  --    show any log splitting (AFAICT; the log file is massive and not
  --    exhaustively checked). I tried with a limit of '1 mb' and that also
  --    seemed fine, though probably unnecessary. I really don't have any
  --    intuition for what a "good value" should be, other than
  --    trial-and-error.
  --
  -- 3. Implement a more complicated "read line" scheme. There are two ways
  --    we could try this:
  --
  --      i. Use the usual hGetNonBlocking, but only take up to the first new
  --         line. Save any remaining data and combine with the next read.
  --         I attempted this once but abandoned it after the logic became
  --         tricky. But maybe this is worth revisiting.
  --
  --      ii. Implement our own primitive hGetLineNonBlocking e.g. read all
  --          bytes up to the first newline, but do not block if we don't have
  --          any data. I have not tried this as these low-level details do not
  --          excite me, but perhaps this is worth investigating.
  --
  -- For now we choose option 2, increase the read-size as it is simple and
  -- appears to work well. We may choose to increase this in the future.
  --
  -- UPDATE: There is another way logs can be split. In some cases, a command
  -- may output a "partial" line, with the expectation to be completed later.
  -- For instance, our test framework outputs lines like:
  --
  --    Some test desc:               OK (0.05s)
  --
  -- But the line is outputted in two steps. First, the text description is
  -- printed. Second, the OK (0.05s) happens _after_ the test completes.
  -- If the time elapsed is long enough to outstrip the poll-interval, then
  -- this log will be broken. Increasing the poll-interval is an option, but
  -- that has other problems. And increasing the read-size will not help here.
  --
  -- The only real solution to this is to implement the more complicated
  -- "read line" scheme described above. Of course this comes with its own
  -- complications and trade-offs.
  def = MkReadSize $ MkBytes 16_000

instance DecodeTOML ReadSize where
  tomlDecoder = parseReadSize tomlDecoder

parseReadSize :: (MonadFail m) => m Text -> m ReadSize
parseReadSize getTxt = do
  byteTxt <- getTxt
  -- Read an unbounded natural, then try to convert to the Int we need.
  case U.parseByteText byteTxt of
    Right b -> case traverse convertIntegral b of
      Left err -> fail $ "Could not convert read-size: " ++ err
      Right b' -> pure $ MkReadSize b'
    Left err -> fail $ "Could not parse --command-log-read-size size: " <> unpack err
