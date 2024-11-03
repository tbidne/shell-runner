{-# LANGUAGE CPP #-}

-- | Provides types for typical "IO" processes.
module Shrun.IO.Handle
  ( -- * Read handle result
    ReadHandleResult (..),

    -- * Reading
    readHandle,
    readHandleRaw,
    readAndUpdateRefFinal,
  )
where

import Data.ByteString qualified as BS
#if MIN_VERSION_base (4, 19, 0)
import Data.List qualified as L
#endif
import Effectful.FileSystem.HandleReader.Static
  ( hGetNonBlocking,
    hIsClosed,
    hIsReadable,
  )
import Effectful.Time.Dynamic (getMonotonicTime)
import GHC.Real (RealFrac (floor))
import Shrun.Configuration.Data.CommandLogging
import Shrun.Data.Text (UnlinedText)
import Shrun.Data.Text qualified as ShrunText
import Shrun.Prelude

-- | Result from reading a handle. The ordering is based on:
--
-- @
-- 'ReadNoData' < 'ReadErr' < 'ReadSuccess'
-- @
--
-- The 'Semigroup' instance is based on this ordering, taking the greatest
-- element. For identical constructors, the left argument is taken.
data ReadHandleResult
  = -- | Error encountered while trying to read a handle.
    ReadErr (NonEmpty UnlinedText)
  | -- | Successfully read data from the handle.
    ReadSuccess (NonEmpty UnlinedText)
  | -- | Error encountered while trying to read a handle, but also have
    -- a successful previous read.
    ReadErrSuccess (NonEmpty UnlinedText) (NonEmpty UnlinedText)
  | -- | Successfully read no data from the handle.
    ReadNoData
  deriving stock (Eq, Show)

instance Semigroup ReadHandleResult where
  ReadSuccess l <> _ = ReadSuccess l
  _ <> ReadSuccess r = ReadSuccess r
  ReadErrSuccess e s <> _ = ReadErrSuccess e s
  _ <> ReadErrSuccess e s = ReadErrSuccess e s
  ReadErr l <> _ = ReadErr l
  _ <> ReadErr r = ReadErr r
  _ <> _ = ReadNoData

instance Monoid ReadHandleResult where
  mempty = ReadNoData

-- NOTE: [Completed vs. Partial Reads]
--
-- readHandle implements "complete vs. partial read detection" for the purposes
-- of making the file output's prettier (i.e. formatting). What follows is an
-- explanation.
--
-- First, we define a _complete_ read as a read that is newline terminated.
-- Otherwise we have a _partial_ read. Notice that we could have multiple
-- complete reads (i.e. multiple newlines in the same read) and a possible
-- partial read, if the entire result does not end in a newline.
--
-- Second, readHandle only implements this strategy when it receives an IORef
-- to store the partial result i.e. if its first param is 'Just'. Otherwise,
-- if it is 'Nothing', we implement the normal strategy which just returns
-- exactly what it reads, up to blockSize bytes.
--
-- Before we get to the implementation strategy, let's briefly look at what
-- we did _not_ do.
--
-- 1. One strategy (and possibly the most natural) is to stop unconditionally
--    appending newlines in the file log formatting, and instead just send
--    what the handle gives us. This would automatically do what we want, so
--    why not?
--
--    Consider what happens if the underlying program does not send _any_
--    newlines (e.g. programs that overwrite the same line). We will end up
--    building a single extremely long line. For this to be reasonable, we'd
--    want to implement some sort of cutoff e.g. "if we haven't read a newline
--    in some time or some data size, insert one".
--
-- 2. We also make no effort to make this sensible with multiple commands.
--    Because we log everything to a single file, there is no sensible way
--    to perform this formatting with multiple commands. To make this actually
--    work we would need to log each command to its own file, which is
--    possible, but would be a very invasive change, possibly with a
--    complicated implementation / interface.
--
-- Onto the implementation.
--
-- When we encounter a partial read, we save it in an IORef. In general, when
-- reading the handle, we check this ref, and if it is non-empty, we prepend
-- it to the first read in the handle. We implement a threshold cutoff, whereby
-- we print it anyway, so that we do not build up a massive string in memory.

type BufferParams =
  ( Tuple4
      (IORef (Maybe UnlinedText)) -- Previous read
      BufferLength -- Buffer length threshold
      BufferTimeout -- Buffer timeout threshold
      (IORef Double) -- Current time
  )

-- | Attempts to read from the handle.
readHandle ::
  ( HasCallStack,
    HandleReader :> es,
    IORefE :> es,
    Time :> es
  ) =>
  Maybe BufferParams ->
  Int ->
  Handle ->
  Eff es ReadHandleResult
readHandle mBufferParams blockSize handle = do
  readHandleRaw blockSize handle >>= \case
    Left err ->
      -- If we encountered an error but are holding onto a previous log,
      -- let's print it too.
      onJust (pure $ ReadErr err) mBufferParams $ \(prevReadRef, _, _, _) ->
        readIORef prevReadRef >>= \case
          Nothing -> pure $ ReadErr err
          Just prevRead -> do
            resetPrevReadRef prevReadRef
            pure $ ReadErrSuccess err (ne prevRead)
    Right bs -> case mBufferParams of
      Nothing -> pure $ case bs of
        "" -> ReadNoData
        -- Empty case probably impossible, for the same reasons in
        -- NOTE: [Non-Empty BS Read].
        cs -> case ShrunText.fromText (decodeUtf8Lenient cs) of
          [] -> ReadNoData
          (x : xs) -> ReadSuccess (x :| xs)
      Just bufferParams -> readAndUpdateRef bufferParams bs

-- | Attempts to read from the handle. Returns Left error or Right
-- success.
readHandleRaw ::
  ( HasCallStack,
    HandleReader :> es
  ) =>
  Int ->
  Handle ->
  Eff es (Either (NonEmpty UnlinedText) ByteString)
readHandleRaw blockSize handle = do
  -- The "nothingIfReady" check and reading step both need to go in the try as
  -- the former can also throw.
  trySync readHandle' <&> \case
    -- unsafeFromTextNE safe because input text is non-empty
    Left ex -> Left $ ShrunText.unsafeFromTextNE $ "HandleException: " <> displayExceptiont ex
    Right x -> x
  where
    readHandle' =
      nothingIfReady >>= \case
        -- unsafeFromTextNE safe because nothingIfReady always returns non-empty.
        Just err -> pure $ Left (ShrunText.unsafeFromTextNE err)
        Nothing ->
          -- NOTE: [Blocking / Streaming output]
          --
          -- hGetNonBlocking is suboptimal because we might take
          -- multiple lines of output, hence log them on a single line, which
          -- is quite ugly. Something like hGetLine would be ideal, but try
          -- as we might we cannot get this to stream properly (logs are
          -- buffered until the process finishes). Thus we have settled on
          -- a workaround: Use the following non blocking call which streams
          -- properly, and manually split the lines ourselves. The block size
          -- should be large enough that we are not likely to cut off a line
          -- prematurely, but obviously this is best-effort.
          Right <$> hGetNonBlocking handle blockSize

    nothingIfReady = do
      -- NOTE: This somewhat torturous logic exists for a reason. We want to
      -- check several conditions before attempting to read from our handle,
      -- but we have to do this in a specific order as some of these boolean
      -- functions will throw exceptions under some circumstances, which we
      -- would like to avoid.
      --
      -- Note that this description comes from experience and reading the
      -- GHC source, so it may not be completely accurate.

      -- hIsClosed does not explicitly throw exceptions so it can be first.
      isClosed <- hIsClosed handle
      if isClosed
        then pure $ Just "Handle closed"
        else do
          -- hIsReadable _does_ throw an exception if the the handle is closed or
          -- "semi-closed". Thus it should go after the hIsClosed check
          -- (GHC explicitly does not export an hSemiClosed).
          isReadable <- hIsReadable handle
          if not isReadable
            then pure $ Just "Handle is not readable"
            else pure Nothing

-- NOTE: [EOF / blocking error] We would like to check hIsEOF (definitely
-- causes errors at the end) and probably hReady as well, but these both
-- block and I have not found a way to invoke them while also streaming
-- the process output (blocks until everything gets dumped at the end).

-- | General handler for combining reads with previous read data.
readAndUpdateRef ::
  forall es.
  ( HasCallStack,
    IORefE :> es,
    Time :> es
  ) =>
  -- | Buffer params.
  BufferParams ->
  -- | Current read.
  ByteString ->
  -- | Result.
  Eff es ReadHandleResult
readAndUpdateRef (prevReadRef, bufferLength, bufferTimeout, bufferWriteTimeRef) =
  readByteStringPrevHandler
    onNoData
    onPartialRead
    onCompletedAndPartialRead
    prevReadRef
  where
    -- 1. No data: Send the prevRead if it exists and breaks the thresholds.
    onNoData :: Eff es ReadHandleResult
    onNoData =
      readIORef prevReadRef
        >>= \case
          Nothing -> pure ReadNoData
          Just prevRead ->
            maybeToReadHandleResult
              <$> prepareSendIfExceedsThresholds (const (pure ())) prevRead

    -- 2. Partial read: Send the data if it breaks the thresholds, prepending
    -- prevRead if it exists.
    onPartialRead :: UnlinedText -> Eff es ReadHandleResult
    onPartialRead finalPartialRead =
      readIORef prevReadRef >>= \case
        Nothing ->
          maybeToReadHandleResult
            <$> prepareSendIfExceedsThresholds updateRef finalPartialRead
        Just prevRead -> do
          let combinedRead = prevRead <> finalPartialRead
          maybeToReadHandleResult
            <$> prepareSendIfExceedsThresholds updateRef combinedRead

    -- 3. Completed reads and partial read.
    onCompletedAndPartialRead :: NonEmpty UnlinedText -> UnlinedText -> Eff es ReadHandleResult
    onCompletedAndPartialRead completedReads finalPartialRead = do
      completedReads' <- mPrependPrevRead prevReadRef completedReads
      finalPartialResult <- prepareSendIfExceedsThresholds updateRef finalPartialRead

      -- We also check that the partial read does not immediately exceed our
      -- thresholds. If it does, no sense storing it, send it now. This is
      -- also consistent with how onPartialRead works.
      let totalRead =
            case finalPartialResult of
              Nothing -> completedReads'
              Just finalRead -> completedReads' <> ne finalRead
      pure $ ReadSuccess totalRead

    -- Turns this text into Just text iff the buffer thresholds are
    -- exceeded.
    prepareSendIfExceedsThresholds ::
      -- Callback for __not__ sending any data. This is used by
      -- onPartialRead to update its IORef, since the reference will be new.
      -- onNoData does not need it since the reference is already up-to-date.
      (UnlinedText -> Eff es ()) ->
      -- The data to check.
      UnlinedText ->
      Eff es (Maybe UnlinedText)
    prepareSendIfExceedsThresholds onNoSend readData = do
      exceeds <- exceedsThreshold readData
      if exceeds
        then do
          resetPrevReadRef'
          currTime <- getMonotonicTime
          writeIORef bufferWriteTimeRef currTime
          pure $ Just readData
        else do
          onNoSend readData
          pure Nothing

    exceedsThreshold :: UnlinedText -> Eff es Bool
    exceedsThreshold t =
      if bufferExceedsLength t
        then pure True
        else bufferExceedsTime

    bufferExceedsLength :: UnlinedText -> Bool
    bufferExceedsLength t = tLen > bufLen
      where
        tLen = ShrunText.length t
        bufLen = bufferLength ^. #unBufferLength

    bufferExceedsTime :: Eff es Bool
    bufferExceedsTime = do
      currTime <- getMonotonicTime
      bufferWriteTime <- readIORef bufferWriteTimeRef

      let diffTime = floor (currTime - bufferWriteTime)

      pure $ diffTime > bufTimeout
      where
        bufTimeout = bufferTimeout ^. #unBufferTimeout % #unTimeout

    resetPrevReadRef' = resetPrevReadRef prevReadRef

    updateRef = writeIORef prevReadRef . Just

    maybeToReadHandleResult Nothing = ReadNoData
    maybeToReadHandleResult (Just read) = ReadSuccess (ne read)

-- | Intended for a final read that handles previous read data.
readAndUpdateRefFinal ::
  forall es.
  ( IORefE :> es
  ) =>
  -- | Previous read.
  IORef (Maybe UnlinedText) ->
  -- | Current read.
  ByteString ->
  -- | Result.
  Eff es ReadHandleResult
readAndUpdateRefFinal prevReadRef =
  readByteStringPrevHandler
    onNoData
    onPartialRead
    onCompletedAndPartialRead
    prevReadRef
  where
    -- 1. No data: Final read, so send off prevRead if it exists, and reset the ref.
    onNoData :: Eff es ReadHandleResult
    onNoData =
      readIORef prevReadRef >>= \case
        Nothing -> resetPrevReadRef' $> ReadNoData
        Just prevRead -> resetPrevReadRef' $> ReadSuccess (ne prevRead)

    -- 2. Partial read: Combine if prevRead exists, send off result.
    onPartialRead :: UnlinedText -> Eff es ReadHandleResult
    onPartialRead finalPartialRead = do
      readIORef prevReadRef >>= \case
        Nothing -> resetPrevReadRef' $> ReadSuccess (ne finalPartialRead)
        Just prevRead -> resetPrevReadRef' $> ReadSuccess (ne $ prevRead <> finalPartialRead)

    -- 3. Completed and partial reads: Combine, send off result.
    onCompletedAndPartialRead :: NonEmpty UnlinedText -> UnlinedText -> Eff es ReadHandleResult
    onCompletedAndPartialRead completedReads finalPartialRead = do
      completedReads' <- mPrependPrevRead prevReadRef completedReads
      resetPrevReadRef'
      pure $ ReadSuccess $ completedReads' <> ne finalPartialRead

    resetPrevReadRef' = resetPrevReadRef prevReadRef

mPrependPrevRead ::
  (IORefE :> es) =>
  IORef (Maybe UnlinedText) ->
  NonEmpty UnlinedText ->
  Eff es (NonEmpty UnlinedText)
mPrependPrevRead ref cr@(r :| rs) =
  readIORef ref >>= \case
    Nothing -> pure cr
    Just prevRead -> resetPrevReadRef' $> prevRead <> r :| rs
  where
    resetPrevReadRef' = resetPrevReadRef ref

-- | Helper for reading a bytestring and handling a previous, partial read.
readByteStringPrevHandler ::
  forall es.
  ( IORefE :> es
  ) =>
  -- | Callback for no data.
  Eff es ReadHandleResult ->
  -- | Callback for a partial, final read.
  (UnlinedText -> Eff es ReadHandleResult) ->
  -- | Callback for completed reads _and_ a partial, final read.
  (NonEmpty UnlinedText -> UnlinedText -> Eff es ReadHandleResult) ->
  -- | Reference that stores the previous, partial read.
  IORef (Maybe UnlinedText) ->
  -- | The bytestring for the current read.
  ByteString ->
  Eff es ReadHandleResult
readByteStringPrevHandler
  onNoData
  onPartialRead
  onCompletedAndPartialRead
  prevReadRef
  bs = case readByteString bs of
    ([], Nothing) -> onNoData
    -- This case is always handled the same: Prepend the prevRead if it
    -- exists, and send all.
    (c : cs, Nothing) -> do
      completedReads <- mPrependPrevRead prevReadRef (c :| cs)
      pure $ ReadSuccess completedReads
    ([], Just finalPartialRead) -> onPartialRead finalPartialRead
    (c : cs, Just finalPartialRead) ->
      onCompletedAndPartialRead (c :| cs) finalPartialRead

-- | Reads a bytestring, distinguishing between _complete_ and _partial_
-- reads. A bytestring is considered _complete_ iff it is terminated with a
-- newline. Otherwise it is _partial_.
--
-- The tuple's left element contains all completed reads. The right element
-- is the final, partial read, if it exists.
readByteString :: ByteString -> Tuple2 (List UnlinedText) (Maybe UnlinedText)
readByteString bs = case BS.unsnoc bs of
  -- 1. Empty: No output
  Nothing -> ([], Nothing)
  -- 2. Non-empty, ends with a newline: This means all reads end with a
  --    newline i.e. are complete.
  --
  -- NOTE: [Non-Empty BS Read]
  --
  -- Note that returning an empty list is __probably__ impossible here, since
  -- unsnoc has already proven that bs is non-empty, and T.lines
  -- (called by decodeRead) only gives an empty string when the input is empty.
  -- So the only possibly way this is empty is if decodeUtf8Lenient somehow
  -- turns non-empty bs into empty text. Probably impossible, but we have this
  -- check since it's better than a runtime error.
  Just (_, 10) -> (decodeRead bs, Nothing)
  -- 3. Non-empty, does not end with a newline: This means the last (and
  --    possibly only) read is partial.
  --
  -- unsnoc will separate the (zero or more) completed read(s) from the final
  -- partial read, which is what we want:
  --
  --     List UnlinedText -> Maybe (List UnlinedText, UnlinedText)
  --
  -- sequenceA will then swap the effects so that we always return the
  -- completed reads(s) and possibly empty final partial read:
  --
  --     Maybe (List UnlinedText, UnlinedText) -> (List UnlinedText, Maybe UnlinedText)
  --
  -- Strictly speaking, the final partial read should always exist, since we
  -- proved the string ends in a non-newline with the case analysis i.e. this
  -- should always return (completed, Just partial).
  --
  -- Unfortunately we cannot prove this to the type system at the moment,
  -- since we need to keep the bytestring together for decoding (hence cannot
  -- use the case-analysis evidence).
  Just (_, _) -> sequenceA $ unsnoc $ decodeRead bs
  where
    decodeRead = ShrunText.fromText . decodeUtf8Lenient

resetPrevReadRef :: (IORefE :> es) => IORef (Maybe a) -> Eff es ()
resetPrevReadRef prevReadRef = writeIORef prevReadRef Nothing

-- TODO: Remove once we are past GHC 9.6
unsnoc :: List a -> Maybe (List a, a)

#if MIN_VERSION_base (4, 19, 0)

unsnoc = L.unsnoc

#else

-- The lazy pattern ~(a, b) is important to be productive on infinite lists
-- and not to be prone to stack overflows.
-- Expressing the recursion via 'foldr' provides for list fusion.
unsnoc = foldr (\x -> Just . maybe ([], x) (\(~(a, b)) -> (x : a, b))) Nothing

#endif

ne :: a -> NonEmpty a
ne x = x :| []
