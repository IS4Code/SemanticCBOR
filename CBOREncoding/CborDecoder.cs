using System;
using System.Buffers.Binary;
using System.Diagnostics;
using System.Diagnostics.CodeAnalysis;
using System.Formats.Cbor;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

namespace IS4.Cbor
{
    /// <summary>
    /// A CBOR decoder used for reading primitive CBOR tokens from data.
    /// </summary>
    /// <typeparam name="TBuffer"><inheritdoc cref="CborDecoderCheckpoint{TBuffer}{TBuffer}" path="/typeparam[@name='TBuffer']"/></typeparam>
    [StructLayout(LayoutKind.Auto)]
    public ref partial struct CborDecoder<TBuffer> where TBuffer : unmanaged
    {
        /// <summary>
        /// A special state indicating the end of an indefinite-length array or map when its proper type is not known.
        /// </summary>
        public const CborReaderState EndIndefiniteLengthCollection = (CborReaderState)254;

        /// <summary>
        /// The minimum size that a chunk before the end of the stream must have to be accepted.
        /// </summary>
        public static readonly int MinNonFinalChunkLength = CborDecoderCheckpoint<TBuffer>.CacheMaxSize;

        /// <summary>
        /// The current state of the decoder.
        /// </summary>
        CborDecoderCheckpoint<TBuffer> s;

        /// <summary>
        /// The current chunk of data processed by the decoder.
        /// </summary>
        readonly ReadOnlySpan<byte> currentChunk;

        /// <summary>
        /// Whether <see cref="currentChunk"/> is the final chunk in the stream.
        /// </summary>
        readonly bool isFinal;

        /// <summary>
        /// The current position in <see cref="currentChunk"/>.
        /// May be negative, in which case it points into <see cref="cache"/> from the end.
        /// </summary>
        int offset;

        /// <summary>
        /// Accesses the previously cached data.
        /// </summary>
        readonly unsafe Span<byte> cache {
            [UnscopedRef]
            get {
                // This is a ref struct so the fields are already fixed
                fixed(void* ptr = &s.Cache)
                {
                    Debug.Assert(s.CacheSize <= CborDecoderCheckpoint<TBuffer>.CacheMaxSize);
                    return new Span<byte>(ptr, s.CacheSize);
                }
            }
        }

        /// <summary>
        /// Accesses the data in <see cref="currentChunk"/> beginning at <see cref="offset"/>.
        /// </summary>
        readonly ReadOnlySpan<byte> chunkView {
            [MethodImpl(MethodImplOptions.AggressiveInlining)]
            get => currentChunk[offset..];
        }

        /// <summary>
        /// Accesses the data in <see cref="cache"/> beginning at <see cref="offset"/>.
        /// </summary>
        readonly ReadOnlySpan<byte> cacheView {
            [UnscopedRef, MethodImpl(MethodImplOptions.AggressiveInlining)]
            get => cache[^-offset..];
        }

        /// <summary>
        /// Obtains either <see cref="chunkView"/> or <see cref="cacheView"/> based on
        /// the sign of <see cref="offset"/>.
        /// </summary>
        readonly ReadOnlySpan<byte> dataView {
            [UnscopedRef, MethodImpl(MethodImplOptions.AggressiveInlining)]
            get => offset >= 0 ? chunkView : cacheView;
        }

        /// <summary>
        /// Accesses the byte of the current data starting at <see cref="offset"/>.
        /// </summary>
        /// <param name="index">The index of the byte.</param>
        /// <returns>The value of the byte.</returns>
        readonly byte this[int index] {
            [MethodImpl(MethodImplOptions.AggressiveInlining)]
            get {
                index += offset;
                if(index < 0)
                {
                    // From cacheView end
                    return cache[s.CacheSize + index];
                }
                return currentChunk[index];
            }
        }

        /// <summary>
        /// When <see cref="State"/> is <see cref="CborReaderState.TextString"/>
        /// or <see cref="CborReaderState.ByteString"/>, contains the value
        /// of the string as bytes.
        /// </summary>
        /// <exception cref="InvalidOperationException">
        /// <see cref="State"/> is not <see cref="CborReaderState.TextString"/>
        /// or <see cref="CborReaderState.ByteString"/>.
        /// </exception>
        public readonly ReadOnlySpan<byte> ValueBytes {
            [UnscopedRef]
            get {
                switch(s.State)
                {
                    case CborReaderState.ByteString:
                    case CborReaderState.TextString:
                        break;
                    default:
                        throw new InvalidOperationException($"Value bytes are exposed only for immediate string values (current state is {s.State}).");
                }
                // Never set to overlap both spans
                return dataView.Slice(0, s.ValueSize);
            }
        }
        
        /// <summary>
        /// The current state of the decoder.
        /// </summary>
        public readonly CborReaderState State {
            [MethodImpl(MethodImplOptions.AggressiveInlining)]
            get => s.State;
        }

        /// <summary>
        /// The initial byte header of the current value.
        /// </summary>
        public readonly CborInitialByte InitialByte {
            [MethodImpl(MethodImplOptions.AggressiveInlining)]
            get => s.InitialByte;
        }

        /// <summary>
        /// Retrieves the raw size of the value. If <see cref="ValueBytes"/>
        /// is available, retrieves its length. In other states that have a value,
        /// retrieves the size of <see cref="RawValue"/>.
        /// </summary>
        public readonly int RawSize {
            [MethodImpl(MethodImplOptions.AggressiveInlining)]
            get => s.ValueSize;
        }

        /// <summary>
        /// The atomic value corresponding to the current state.
        /// For definite-length collections, this value stores the number
        /// of the elements in the collection.
        /// </summary>
        public readonly ulong RawValue {
            [MethodImpl(MethodImplOptions.AggressiveInlining)]
            get => s.RawValue;
        }

        /// <summary>
        /// Whether the current value is marked as having indefinite length in the data.
        /// </summary>
        public readonly bool IsIndefiniteLength {
            [MethodImpl(MethodImplOptions.AggressiveInlining)]
            get => s.InitialByte.AdditionalInfo == CborAdditionalInfo.IndefiniteLength;
        }

        /// <summary>
        /// Whether information about nested collections is preserved.
        /// </summary>
        public readonly bool PreservesNestedCollections {
            [MethodImpl(MethodImplOptions.AggressiveInlining)]
            get => s.CollectionStates.IsInitialized;
        }

        /// <summary>
        /// The current depth of nested collections, if <see cref="PreservesNestedCollections"/>
        /// is <see langword="true"/>.
        /// </summary>
        public readonly int CurrentDepth {
            [MethodImpl(MethodImplOptions.AggressiveInlining)]
            get => s.CollectionStates.Count;
        }

        /// <summary>
        /// Whether the current value is a key in a map.
        /// </summary>
        public readonly bool IsMapKey {
            [MethodImpl(MethodImplOptions.AggressiveInlining)]
            get => !s.CollectionState.NextIsValue;
        }

        /// <summary>
        /// The number of remaining elements in the current collection, if available.
        /// </summary>
        public readonly ulong? RemainingCollectionElements {
            [MethodImpl(MethodImplOptions.AggressiveInlining)]
            get => PreservesNestedCollections && s.CollectionState.CollectionByte.AdditionalInfo != CborAdditionalInfo.IndefiniteLength
                ? s.CollectionState.RemainingElements
                : null;
        }

        /// <summary>
        /// Constructs a new instance of the decoder from the first chunk of the data stream.
        /// </summary>
        /// <param name="initialChunk">The first chunk of the data.</param>
        /// <param name="isFinalChunk">Whether <paramref name="initialChunk"/> is the only chunk in the data stream.</param>
        /// <param name="preserveNestedCollections">The value of <see cref="PreservesNestedCollections"/>.</param>
        public CborDecoder(ReadOnlySpan<byte> initialChunk, bool isFinalChunk, bool preserveNestedCollections = false)
        {
            s = default;
            if(preserveNestedCollections)
            {
                s.CollectionStates = new();
            }
            s.CollectionState.NextIsValue = true;
            currentChunk = initialChunk;
            isFinal = isFinalChunk;
            offset = 0;

            Initialize(nameof(initialChunk));
        }

        /// <summary>
        /// Constructs a new instance of the decoder from a previous state and the next chunk of the data stream.
        /// </summary>
        /// <param name="previousCheckpoint">
        /// The checkpoint produced by a previous instance of <see cref="CborDecoder"/>,
        /// obtained from <see cref="TryGetCheckpoint(out CborDecoderCheckpoint)"/>
        /// or <see cref="CborCheckpointException"/>.
        /// </param>
        /// <param name="nextChunk">The next chunk of the data.</param>
        /// <param name="isFinalChunk">Whether <paramref name="nextChunk"/> is the last chunk in the data stream.</param>
        public CborDecoder(scoped in CborDecoderCheckpoint<TBuffer> previousCheckpoint, ReadOnlySpan<byte> nextChunk, bool isFinalChunk)
        {
            s = previousCheckpoint;
            currentChunk = nextChunk;
            isFinal = isFinalChunk;
            offset = -s.CacheSize;

            Initialize(nameof(nextChunk));
        }

        /// <summary>
        /// Starts decoding the data.
        /// </summary>
        /// <param name="paramName">The name of the parameter that holds the data.</param>
        /// <exception cref="ArgumentException">Thrown when invalid input is given.</exception>
        private void Initialize(string paramName)
        {
            if(MinNonFinalChunkLength < CborDefaultBuffer.Size)
            {
                throw new NotSupportedException($"Type {typeof(TBuffer)} used as the cache array must take at least {CborDefaultBuffer.Size} bytes to be usable.");
            }
            if(!HasEnoughSpace)
            {
                throw new ArgumentException($"Non-final input data chunk must have at least {MinNonFinalChunkLength} bytes.", paramName);
            }
            Decode();
        }

        /// <summary>
        /// Retrieves the state corresponding to the next value in the data stream.
        /// </summary>
        /// <returns>The value of <see cref="State"/> after decoding the next value.</returns>
        /// <exception cref="CborContentException">
        /// Thrown when the following data is not well-formed CBOR.
        /// </exception>
        /// <exception cref="CborCheckpointException">
        /// Thrown when there is not enough data for successful decoding.
        /// <see cref="CborCheckpointException.Checkpoint"/> stores the checkpoint
        /// to resume the decoding via <see cref="CborDecoder.CborDecoder(in CborDecoderCheckpoint, ReadOnlySpan{byte}, bool)"/>.
        /// </exception>
        public CborReaderState PeekState()
        {
            Decode();
            return s.State;
        }

        /// <summary>
        /// Whether there is enough space for decoding the remaining data.
        /// </summary>
        readonly bool HasEnoughSpace {
            [MethodImpl(MethodImplOptions.AggressiveInlining)]
            get => isFinal || currentChunk.Length - offset >= MinNonFinalChunkLength;
        }

        /// <summary>
        /// Whether a chunked string is currently processed.
        /// </summary>
        readonly bool IsChunkedString {
            [MethodImpl(MethodImplOptions.AggressiveInlining)]
            get => s.StringChunk is not StringChunkState.None;
        }

        /// <summary>
        /// Whether the decoder is currently after a string chunk
        /// and needs to decode the next one.
        /// </summary>
        readonly bool ShouldDecodeAfterStringChunk {
            [MethodImpl(MethodImplOptions.AggressiveInlining)]
            get => IsChunkedString && s.ValueSize == 0;
        }

        /// <summary>
        /// Whether the decoder is currently in an indeterminate state.
        /// </summary>
        readonly bool ShouldCallDecode {
            [MethodImpl(MethodImplOptions.AggressiveInlining)]
            get => s.State == CborReaderState.Undefined || ShouldDecodeAfterStringChunk;
        }

        /// <summary>
        /// Whether the current value is located in an indefinite-length string.
        /// </summary>
        readonly bool IsInsideIndefiniteLengthString {
            [MethodImpl(MethodImplOptions.AggressiveInlining)]
            get => s.ContainerType is CborMajorType.TextString or CborMajorType.ByteString;
        }

        /// <summary>
        /// Ensures <see cref="HasEnoughSpace"/>, or throws <see cref="NeedsMoreData"/>.
        /// </summary>
        /// <exception cref="CborCheckpointException">
        /// More data needs to be provided to the decoder.
        /// </exception>
        void EnsureEnoughSpace()
        {
            if(!HasEnoughSpace)
            {
                throw NeedsMoreData();
            }
        }

        /// <summary>
        /// Moves the decoder to the next value in the data stream.
        /// </summary>
        /// <exception cref="CborContentException">
        /// Thrown when the following data is not well-formed CBOR.
        /// </exception>
        /// <exception cref="CborCheckpointException">
        /// Thrown when there is not enough data for successful decoding.
        /// <see cref="CborCheckpointException.Checkpoint"/> stores the checkpoint
        /// to resume the decoding via <see cref="CborDecoder.CborDecoder(in CborDecoderCheckpoint, ReadOnlySpan{byte}, bool)"/>.
        /// </exception>
        public void Advance()
        {
            if(ShouldCallDecode)
            {
                // We need to decode the value anyway
                Decode();
            }
            switch(s.State)
            {
                case CborReaderState.Finished:
                    // Reached the end
                    return;

                case CborReaderState.StartIndefiniteLengthByteString
                or CborReaderState.StartIndefiniteLengthTextString
                when IsChunkedString:
                    // When there is chunk string overlap, this simulates
                    // advancing to a next definite-sized string.
                    s.State--;
                    return;

                case CborReaderState.EndIndefiniteLengthByteString
                or CborReaderState.EndIndefiniteLengthTextString
                when IsChunkedString:
                    // Reset overlap information
                    s.StringChunk = StringChunkState.None;
                    break;

                case CborReaderState.TextString:
                case CborReaderState.ByteString:
                    // Move past the value
                    offset += s.ValueSize;
                    if(!IsChunkedString)
                    {
                        // This was a normal string value
                        break;
                    }
                    // There is more data
                    Debug.Assert(s.ValueSize != 0);
                    s.RawValue -= unchecked((uint)s.ValueSize);
                    // Request decoding next chunk
                    s.ValueSize = 0;
                    if(s.StringChunk != StringChunkState.Contiguous)
                    {
                        // This was a cache boundary overlap
                        Debug.Assert(offset == 0);
                        offset = (byte)s.StringChunk;
                    }
                    if(s.RawValue == 0)
                    {
                        // Simulate end of string if not within indefinite
                        if(!IsInsideIndefiniteLengthString)
                        {
                            s.State += 2;
                        }
                        else
                        {
                            // Reset overlap information
                            s.StringChunk = StringChunkState.None;
                            break;
                        }
                    }
                    return;

                case CborReaderState.StartArray:
                case CborReaderState.StartMap:
                    // Push new state
                    if(PreservesNestedCollections)
                    {
                        s.CollectionStates.Push(s.CollectionState);
                        s.CollectionState.CollectionByte = s.InitialByte;
                        s.CollectionState.NextIsValue = true;
                        s.CollectionState.RemainingElements = s.ValueSize == -1 ? UInt64.MaxValue : s.RawValue;
                    }
                    break;

                case CborReaderState.EndArray:
                case CborReaderState.EndMap:
                    // Pop old state
                    if(!s.CollectionStates.TryPop(out s.CollectionState))
                    {
                        throw new InvalidOperationException("Unbalanced collection states.");
                    }
                    break;
            }
            if(PreservesNestedCollections && s.CollectionState.RemainingElements == 0)
            {
                // Virtual end state of a definite-length collection
                switch(s.CollectionState.CollectionByte.MajorType)
                {
                    case CborMajorType.Array:
                        s.State = CborReaderState.EndArray;
                        return;
                    case CborMajorType.Map:
                        s.State = CborReaderState.EndMap;
                        return;
                }
            }
            s.State = CborReaderState.Undefined;
        }

        /// <summary>
        /// Attempts to store the state of the decoder
        /// in a <see cref="CborDecoderCheckpoint"/> instance.
        /// </summary>
        /// <param name="checkpoint">
        /// The variable to store the checkpoint in. If <see langword="false"/>
        /// is returned, the checkpoint is left uninitialized.
        /// </param>
        /// <returns>Whether a new checkpoint was produced.</returns>
        /// <remarks>
        /// When <see langword="true"/> is returned, the current instance,
        /// is no longer in a usable state.
        /// </remarks>
        public bool TryGetCheckpoint(out CborDecoderCheckpoint<TBuffer> checkpoint)
        {
            if(HasEnoughSpace)
            {
                // Not needed
                Unsafe.SkipInit(out checkpoint);
                // Initialize reference
                checkpoint.CollectionStates = default;
                return false;
            }

            StoreToCheckpoint(out checkpoint);
            return true;
        }

        /// <summary>
        /// Packs the current decoder state into a <see cref="CborDecoderCheckpoint"/>.
        /// </summary>
        /// <param name="checkpoint">The variable to store the state in.</param>
        /// <exception cref="InvalidOperationException">
        /// Thrown when the remaining data could not fit in the checkpoint's cache.
        /// </exception>
        internal void StoreToCheckpoint(out CborDecoderCheckpoint<TBuffer> checkpoint)
        {
            // Copy remaining into cache
            var remaining = chunkView;
            if(remaining.Length > CborDecoderCheckpoint<TBuffer>.CacheMaxSize)
            {
                throw new InvalidOperationException("Remaining chunk data is too big to fit into a checkpoint.");
            }
            s.CacheSize = remaining.Length;
            remaining.CopyTo(cache);

            // Return a copy
            checkpoint = s;

            // Move this instance to the end
            s.State = CborReaderState.Finished;
            s.CacheSize = 0;
            s.CollectionStates = default;
            offset = currentChunk.Length;
        }

        /// <summary>
        /// If the following value needs decoding, sets the state of the
        /// instance accordingly.
        /// </summary>
        private void Decode()
        {
            if(s.State != CborReaderState.Undefined)
            {
                if(!ShouldDecodeAfterStringChunk)
                {
                    // State is fully known
                    return;
                }
                // Decode after reading string chunk
                EnsureEnoughSpace();
                ProcessStringChunk(StringChunkState.Contiguous);
                Debug.Assert(IsChunkedString);
                return;
            }

            Debug.Assert(!IsChunkedString);
            EnsureEnoughSpace();

            if(currentChunk.Length - offset == 0)
            {
                // End of data
                Debug.Assert(isFinal);
                s.State = CborReaderState.Finished;
                return;
            }

            s.InitialByte = new(this[0]);
            s.State = s.InitialByte.MajorType switch
            {
                CborMajorType.UnsignedInteger => CborReaderState.UnsignedInteger,
                CborMajorType.NegativeInteger => CborReaderState.NegativeInteger,
                CborMajorType.ByteString => CborReaderState.ByteString,
                CborMajorType.TextString => CborReaderState.TextString,
                CborMajorType.Array => CborReaderState.StartArray,
                CborMajorType.Map => CborReaderState.StartMap,
                CborMajorType.Tag => CborReaderState.Tag,
                CborMajorType.Simple => CborReaderState.SimpleValue,
                _ => throw new CborContentException($"Unknown major type {s.InitialByte.MajorType}.")
            };
            s.ValueSize = s.InitialByte.AdditionalInfo switch
            {
                >= 0 and < CborAdditionalInfo.Additional8BitData => 0,
                CborAdditionalInfo.Additional8BitData => 1,
                CborAdditionalInfo.Additional16BitData => 2,
                CborAdditionalInfo.Additional32BitData => 4,
                CborAdditionalInfo.Additional64BitData => 8,
                CborAdditionalInfo.IndefiniteLength => -1,
                _ => throw new CborContentException($"Unrecognized additional information {s.InitialByte.AdditionalInfo}.")
            };

            // Move past the byte; (8 bytes remaining for non-final chunks)
            offset++;

            if(isFinal && currentChunk.Length - offset < s.ValueSize)
            {
                // Missing final data
                throw new CborContentException($"Missing full argument for {s.InitialByte.MajorType}, {s.InitialByte.AdditionalInfo}.");
            }

            if(s.ValueSize > 0)
            {
                if(unchecked((uint)offset > (uint)-s.ValueSize))
                {
                    // Reading cross boundary
                    s.RawValue = ReadBoundaryBytes(s.ValueSize);
                }
                else
                {
                    // Reading from one of the views
                    s.RawValue = ReadBytes(dataView, s.ValueSize);
                }

                // Advance
                offset += s.ValueSize;
            }
            else
            {
                // Taken from additional info
                s.RawValue = (byte)s.InitialByte.AdditionalInfo;
            }

            if(s.InitialByte.Value == CborInitialByte.IndefiniteLengthBreakByte)
            {
                // Break code
                switch(s.ContainerType)
                {
                    case CborMajorType.TextString:
                        s.State = CborReaderState.EndIndefiniteLengthTextString;
                        return;
                    case CborMajorType.ByteString:
                        s.State = CborReaderState.EndIndefiniteLengthByteString;
                        return;
                    case CborMajorType.Tag:
                        throw new CborContentException("A tag value expected.");
                    default:
                        if(!PreservesNestedCollections)
                        {
                            s.State = EndIndefiniteLengthCollection;
                            return;
                        }
                        if(s.CollectionState.CollectionByte.AdditionalInfo != CborAdditionalInfo.IndefiniteLength)
                        {
                            throw new CborContentException("Break code encountered in a definite-length collection.");
                        }
                        s.State = s.CollectionState.CollectionByte.MajorType switch
                        {
                            CborMajorType.Array => CborReaderState.EndArray,
                            CborMajorType.Map => CborReaderState.EndMap,
                            _ => throw new CborContentException("Break code encountered while not in a collection.")
                        };
                        return;
                }
            }
            // A normal value

            if(IsInsideIndefiniteLengthString)
            {
                // A value in an indefinite-length string
                if(s.InitialByte.MajorType != s.ContainerType)
                {
                    throw new CborContentException($"Encountered wrong indefinite-length string chunk type ({s.InitialByte.MajorType} but {s.ContainerType} required).");
                }
            }
            else if(s.State == CborReaderState.Tag)
            {
                // Tag value expected
                s.ContainerType = CborMajorType.Tag;
            }
            else
            {
                // A singular value
                s.ContainerType = 0;
                switch(s.CollectionState.CollectionByte.MajorType)
                {
                    // Update collection state if any
                    case CborMajorType.Array:
                        if(s.CollectionState.CollectionByte.AdditionalInfo != CborAdditionalInfo.IndefiniteLength)
                        {
                            // Decrement remaining elements
                            --s.CollectionState.RemainingElements;
                        }
                        break;
                    case CborMajorType.Map:
                        if(s.CollectionState.NextIsValue)
                        {
                            // Move to value state
                            s.CollectionState.NextIsValue = false;
                        }
                        else
                        {
                            // Move to key state
                            s.CollectionState.NextIsValue = true;
                            if(s.CollectionState.CollectionByte.AdditionalInfo != CborAdditionalInfo.IndefiniteLength)
                            {
                                // Decrement remaining elements
                                if(--s.CollectionState.RemainingElements != 0)
                                {
                                    // Move to key state
                                    s.CollectionState.NextIsValue = true;
                                }
                            }
                        }
                        break;
                }
            }

            switch(s.State)
            {
                case CborReaderState.TextString:
                case CborReaderState.ByteString:
                    if(s.ValueSize == -1)
                    {
                        // Indefinite length
                        if(IsInsideIndefiniteLengthString)
                        {
                            // Nested
                            throw new CborContentException($"Indefinite-length {s.State} encountered inside another.");
                        }
                        // Store type and move to the Start state
                        s.ContainerType = s.InitialByte.MajorType;
                        s.State++;
                        return;
                    }
                    // Finite length but maybe split over multiple spans
                    if(!ProcessStringChunk(StringChunkState.None))
                    {
                        Debug.Assert(IsChunkedString);
                        // Simulate indefinite length if not already
                        if(!IsInsideIndefiniteLengthString)
                        {
                            s.State++;
                        }
                    }
                    return;
                case CborReaderState.StartMap:
                case CborReaderState.StartArray:
                    // Indefinite-length permitted
                    return;
            }
            // Not a string/array/map

            if(s.ValueSize == -1)
            {
                throw new CborContentException($"Invalid value byte 0x{s.InitialByte.Value:X2} (indefinite length specified for atomic value).");
            }

            if(s.State != CborReaderState.SimpleValue)
            {
                // All done
                return;
            }

            // Decode remaining simple values
            switch(s.ValueSize)
            {
                case 1:
                    if(s.ValueSize == 1 && s.RawValue < 32)
                    {
                        // Explicitly disallowed
                        throw new CborContentException($"Invalid byte sequence 0x{s.InitialByte.Value:X2}{s.RawValue:X2} (primitive value stored in two bytes).");
                    }
                    break;
                case 2:
                    s.State = CborReaderState.HalfPrecisionFloat;
                    return;
                case 4:
                    s.State = CborReaderState.SinglePrecisionFloat;
                    return;
                case 8:
                    s.State = CborReaderState.DoublePrecisionFloat;
                    return;
            }

            // Plain simple value
            switch((CborSimpleValue)s.RawValue)
            {
                case CborSimpleValue.False:
                    s.State = CborReaderState.Boolean;
                    s.RawValue = 0;
                    return;
                case CborSimpleValue.True:
                    s.State = CborReaderState.Boolean;
                    s.RawValue = 1;
                    return;
                case CborSimpleValue.Null:
                    s.State = CborReaderState.Null;
                    return;
                case CborSimpleValue.Undefined:
                    return;
                default:
                    // Unassigned simple value
                    return;
            }
        }

        /// <summary>
        /// Processes the next chunk of the current string and sets the state
        /// accordingly.
        /// </summary>
        /// <param name="resetToState">
        /// The <see cref="CborDecoderCheckpoint.StringChunk"/> value to reset to
        /// when this is the final chunk of the string.
        /// </param>
        /// <returns>Whether this is the final chunk of the string.</returns>
        bool ProcessStringChunk(StringChunkState resetToState)
        {
            if(offset >= 0)
            {
                // Starts within the chunk
                if(s.RawValue <= unchecked((ulong)(currentChunk.Length - offset)))
                {
                    // Ends within the chunk - safe to return
                    s.ValueSize = unchecked((int)s.RawValue);
                    s.StringChunk = resetToState;
                    return true;
                }
                if(isFinal)
                {
                    // There is no data past the end of the chunk
                    throw UnexpectedEnd();
                }
                // Multiple spans will need to be reported for the next chunks
                s.ValueSize = FindValidStringEnd(chunkView, out _);
                if(s.ValueSize != 0)
                {
                    // Some bytes will need to be taken from the next chunk,
                    // but by that point they will be located in the cache
                    s.StringChunk = StringChunkState.Contiguous;
                    return false;
                }

                // Offset is close to the end, but the data was not refreshed
                throw NeedsMoreData();
            }

            // Starts within the cached range
            if(s.RawValue <= (ulong)CborDecoderCheckpoint<TBuffer>.CacheMaxSize && unchecked(offset + (int)s.RawValue <= 0))
            {
                // Ends within the cached range - safe to return
                s.ValueSize = unchecked((int)s.RawValue);
                s.StringChunk = resetToState;
                return true;
            }
            // Crosses the cache boundary - multiple spans will need to be reported
            s.ValueSize = FindValidStringEnd(cacheView, out int overlapSize);
            if(s.ValueSize != 0)
            {
                // We can retrieve the initial bytes from the cache for now
                s.StringChunk = StringChunkState.Contiguous;
                return false;
            }
            // A Unicode character crosses the boundary - it needs to be reported as a whole
            Debug.Assert(offset > -overlapSize);
            // Offset is -3..-1 (overlap size is only at most 4)

            // Store the reported size of the character
            s.ValueSize = overlapSize;

            // Can the whole character fit before cacheView?
            bool cacheNotBigEnough = overlapSize - offset > s.CacheSize;
            // If not, store it temporarily on the stack
            var characterStorage = cacheNotBigEnough ? stackalloc byte[overlapSize] : cache.Slice(overlapSize);

            // Move the cached part to the beginning (filling at most ..3 so no overlap in the spans)
            cacheView.CopyTo(characterStorage);

            // Get bytes from the current chunk (at most 3)
            overlapSize += offset;

            if(overlapSize > currentChunk.Length)
            {
                // String size reported but data missing
                Debug.Assert(isFinal);
                throw UnexpectedEnd();
            }

            // Fill the rest of the character's bytes
            currentChunk[..overlapSize].CopyTo(characterStorage[^overlapSize..]);

            if(cacheNotBigEnough)
            {
                // Copy back to the cache
                characterStorage.CopyTo(cache);
            }

            // Update the size of the cache
            s.CacheSize = s.ValueSize;

            // Update offset to point to the beginning of the cache
            offset = -s.ValueSize;

            // Store the remaining overlap (1..3) - this will be the future offset
            s.StringChunk = (StringChunkState)overlapSize;
            return false;
        }

        /// <summary>
        /// Looks for the end of a string to be safely returned as a single chunk.
        /// </summary>
        /// <param name="data">The input string.</param>
        /// <param name="nextCharacterSize">
        /// The expected length of the Unicode character that crosses the end of <paramref name="data"/>.
        /// </param>
        /// <returns>
        /// The last position in <paramref name="data"/> that is safe to end it,
        /// so that no UTF-8 character sequence crosses the end.
        /// </returns>
        /// <exception cref="InvalidOperationException">
        /// Thrown when the decoder is not processing a string value.
        /// </exception>
        readonly int FindValidStringEnd(ReadOnlySpan<byte> data, out int nextCharacterSize)
        {
            switch(s.State)
            {
                case CborReaderState.ByteString:
                case CborReaderState.StartIndefiniteLengthByteString:
                    nextCharacterSize = 0;
                    return data.Length;
                case CborReaderState.TextString:
                case CborReaderState.StartIndefiniteLengthTextString:
                    break;
                default:
                    throw new InvalidOperationException($"String value cannot be retrieved in state {s.State}.");
            }
            int dataOffset = data.Length;
            while(dataOffset != 0)
            {
                dataOffset--;
                byte b = data[dataOffset];
                if(b <= 0x7F)
                {
                    // 0xxxxxxx
                    nextCharacterSize = 0;
                    return dataOffset + 1;
                }
                else if(b <= 0xBF)
                {
                    // 10xxxxxx
                    if(dataOffset == 0)
                    {
                        throw new CborContentException($"Unexpected UTF-8 byte 0x{b:X2} at position 0.");
                    }
                    continue;
                }
                int sequenceSize;
                if(b <= 0xDF)
                {
                    // 110xxxxx 10xxxxxx
                    sequenceSize = 2;
                }
                else if(b <= 0xEF)
                {
                    // 1110xxxx 10xxxxxx 10xxxxxx
                    sequenceSize = 3;
                }
                else if(b <= 0xF7)
                {
                    // 11110xxx 10xxxxxx 10xxxxxx 10xxxxxx
                    sequenceSize = 4;
                }
                else
                {
                    throw new CborContentException($"Invalid UTF-8 byte 0x{b:X2} at position {dataOffset}.");
                }
                int sequenceEnd = dataOffset + sequenceSize;
                int sequenceEndOffset = sequenceEnd - data.Length;
                if(sequenceEndOffset > 0)
                {
                    // The character ends past the span
                    nextCharacterSize = sequenceSize;
                    if(unchecked((uint)sequenceEnd) > s.RawValue)
                    {
                        // The character cannot fit in the reported string size 
                        throw new CborContentException($"Unexpected UTF-8 string end: byte 0x{b:X2} at position {dataOffset} needs {sequenceSize - 1} bytes to follow, but there are only {unchecked((int)s.RawValue - dataOffset - 1)}.");
                    }
                    return dataOffset;
                }
                else if(sequenceEndOffset == 0)
                {
                    // The character ends with the span
                    nextCharacterSize = 0;
                    return data.Length;
                }
                else
                {
                    // The character ends before the span,
                    // but then the next byte was not recognized
                    // as a beginning of a new character.
                    dataOffset += sequenceSize;
                    b = data[dataOffset];
                    throw new CborContentException($"Unexpected UTF-8 byte 0x{b:X2} at position {dataOffset}.");
                }
            }
            // Empty string
            nextCharacterSize = 0;
            return 0;
        }

        /// <summary>
        /// Reads an atomic value at the boundary of <see cref="cache"/> and <see cref="currentChunk"/>.
        /// </summary>
        /// <param name="size">The size of the value.</param>
        /// <returns>The value at the current position.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        readonly ulong ReadBoundaryBytes(int size)
        {
            return size switch
            {
                1 => this[0],
                2 => (uint)this[0] << 8 | this[1],
                4 => (uint)this[0] << 24 | (uint)this[1] << 16 | (uint)this[2] << 8 | this[3],
                8 => (ulong)this[0] << 56 | (ulong)this[1] << 48 | (ulong)this[2] << 40 | (ulong)this[3] << 32 | (ulong)this[4] << 24 | (ulong)this[5] << 16 | (ulong)this[6] << 8 | this[7],
                _ => 0
            };
        }

        /// <summary>
        /// Reads an atomic value from <paramref name="data"/>.
        /// </summary>
        /// <param name="data">The bytes to read the value from.</param>
        /// <param name="size">The size of the value.</param>
        /// <returns>The value at the beginning of <paramref name="data"/>.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        static ulong ReadBytes(ReadOnlySpan<byte> data, int size)
        {
            return size switch
            {
                1 => data[0],
                2 => BinaryPrimitives.ReadUInt16BigEndian(data),
                4 => BinaryPrimitives.ReadUInt32BigEndian(data),
                8 => BinaryPrimitives.ReadUInt64BigEndian(data),
                _ => 0
            };
        }

        /// <summary>
        /// Constructs an exception to throw when the data stream is ending but more data is needed.
        /// </summary>
        /// <returns>The new exception.</returns>
        readonly CborContentException UnexpectedEnd()
        {
            return new CborContentException($"Unexpected end of data for {s.State}.");
        }

        /// <summary>
        /// Constructs an exception to throw when the next chunk of the data stream needs to be provided.
        /// </summary>
        /// <returns>The new exception.</returns>
        CborCheckpointException<TBuffer> NeedsMoreData()
        {
            return new CborCheckpointException<TBuffer>("A new chunk needs to be provided to continue reading using the reader's checkpoint.", ref this);
        }
    }

    /// <summary>
    /// An opaque state produced by <see cref="CborDecoder"/> to share
    /// across chunks of the input data stream.
    /// </summary>
    /// <typeparam name="TBuffer">
    /// The inline array type used to hold cached bytes.
    /// Must have at least <see cref="CborDefaultBuffer.Size"/> bytes.
    /// </typeparam>
    [StructLayout(LayoutKind.Auto)]
    public struct CborDecoderCheckpoint<TBuffer> where TBuffer : unmanaged
    {
        /// <summary>
        /// The maximum size of <see cref="Cache"/>.
        /// </summary>
        internal static readonly int CacheMaxSize = Unsafe.SizeOf<TBuffer>();

        /// <summary>
        /// The backing field for <see cref="State"/>.
        /// </summary>
        byte _state;

        /// <summary>
        /// The current state of the decoder.
        /// </summary>
        internal CborReaderState State {
            [MethodImpl(MethodImplOptions.AggressiveInlining)]
            readonly get => (CborReaderState)_state;
            [MethodImpl(MethodImplOptions.AggressiveInlining)]
            set => _state = checked((byte)value);
        }

        /// <summary>
        /// The initial byte of the current value.
        /// </summary>
        internal CborInitialByte InitialByte;

        /// <summary>
        /// The state of the chunked string decoding.
        /// </summary>
        internal StringChunkState StringChunk;

        /// <summary>
        /// The currently open container type.
        /// </summary>
        internal CborMajorType ContainerType;

        /// <summary>
        /// The collection state at this depth.
        /// </summary>
        internal CborCollectionState CollectionState;

        /// <summary>
        /// Collection states at previous depths.
        /// </summary>
        internal ValueStack<CborCollectionState> CollectionStates;

        /// <summary>
        /// The size of the decoded value.
        /// </summary>
        internal int ValueSize;

        /// <summary>
        /// The raw atomic value stored in the input data.
        /// </summary>
        internal ulong RawValue;

        /// <summary>
        /// The number of valid initial bytes in <see cref="Cache"/>.
        /// </summary>
        internal int CacheSize;

        /// <summary>
        /// The cached bytes from the previous data chunk,
        /// or other values that need to be reported
        /// as a contiguous range.
        /// </summary>
        internal TBuffer Cache;
    }

    /// <summary>
    /// An unmanaged type of size <see cref="Size"/> used to hold temporary byte data.
    /// </summary>
    [StructLayout(LayoutKind.Explicit, Size = Size)]
    public struct CborDefaultBuffer
    {
        /// <summary>
        /// The size of this cache type.
        /// </summary>
        public const int Size = sizeof(byte) + sizeof(ulong);
    }

    /// <summary>
    /// Stores the information about the collection at this level.
    /// </summary>
    [StructLayout(LayoutKind.Auto)]
    internal struct CborCollectionState
    {
        public CborInitialByte CollectionByte;
        public bool NextIsValue;
        public ulong RemainingElements;
    }

    /// <summary>
    /// The state of chunked string processing.
    /// </summary>
    enum StringChunkState : byte
    {
        /// <summary>
        /// No chunk string has occurred.
        /// </summary>
        None = 0,

        /// <summary>
        /// A string chunk character overlaps 1 byte into the next data chunk.
        /// </summary>
        Over1 = 1,

        /// <summary>
        /// A string chunk character overlaps 2 bytes into the next data chunk.
        /// </summary>
        Over2 = 2,

        /// <summary>
        /// A string chunk character overlaps 3 bytes into the next data chunk.
        /// </summary>
        Over3 = 3,

        /// <summary>
        /// A string chunk is encountered without an overlap.
        /// </summary>
        Contiguous = 10
    }

    /// <summary>
    /// The major type of a CBOR value.
    /// </summary>
    public enum CborMajorType : byte
    {
        /// <summary>
        /// An unsigned integer.
        /// </summary>
        UnsignedInteger = 0 << CborInitialByte.MajorTypeShift,

        /// <summary>
        /// A negative integer.
        /// </summary>
        NegativeInteger = 1 << CborInitialByte.MajorTypeShift,

        /// <summary>
        /// A string of bytes.
        /// </summary>
        ByteString = 2 << CborInitialByte.MajorTypeShift,

        /// <summary>
        /// A string of UTF-8 characters.
        /// </summary>
        TextString = 3 << CborInitialByte.MajorTypeShift,

        /// <summary>
        /// An array of values.
        /// </summary>
        Array = 4 << CborInitialByte.MajorTypeShift,

        /// <summary>
        /// A map of key-value pairs.
        /// </summary>
        Map = 5 << CborInitialByte.MajorTypeShift,

        /// <summary>
        /// A tagged value.
        /// </summary>
        Tag = 6 << CborInitialByte.MajorTypeShift,

        /// <summary>
        /// A simple/float value.
        /// </summary>
        Simple = 7 << CborInitialByte.MajorTypeShift,
    }

    /// <summary>
    /// The additional information about a CBOR value.
    /// </summary>
    public enum CborAdditionalInfo : byte
    {
        /// <summary>
        /// The actual value is stored in the next byte.
        /// </summary>
        Additional8BitData = 24,

        /// <summary>
        /// The actual value is stored in the next 2 bytes.
        /// </summary>
        Additional16BitData = 25,

        /// <summary>
        /// The actual value is stored in the next 4 bytes.
        /// </summary>
        Additional32BitData = 26,

        /// <summary>
        /// The actual value is stored in the next 8 bytes.
        /// </summary>
        Additional64BitData = 27,

        /// <summary>
        /// The length of the sequence is indefinite.
        /// </summary>
        IndefiniteLength = 31,
    }

    /// <summary>
    /// Represents the initial byte of a CBOR value.
    /// </summary>
    [StructLayout(LayoutKind.Sequential, Pack = 1, Size = 1)]
    public readonly struct CborInitialByte
    {
        /// <summary>
        /// The value indicating the break of an indefinite-length sequence.
        /// </summary>
        public const byte IndefiniteLengthBreakByte = 0xFF;

        /// <summary>
        /// The position of the major type in the byte from the right.
        /// </summary>
        public const int MajorTypeShift = 5;

        /// <summary>
        /// The mask of the <see cref="CborAdditionalInfo"/> value.
        /// </summary>
        public const byte AdditionalInformationMask = (1 << MajorTypeShift) - 1;

        /// <summary>
        /// The raw value of the byte.
        /// </summary>
        public byte Value { get; }

        /// <summary>
        /// The major type part of the byte.
        /// </summary>
        public CborMajorType MajorType => (CborMajorType)(Value & ~AdditionalInformationMask);

        /// <summary>
        /// The additional information part of the byte.
        /// </summary>
        public CborAdditionalInfo AdditionalInfo => (CborAdditionalInfo)(Value & AdditionalInformationMask);

        /// <summary>
        /// Constructs a new value using the <see cref="CborMajorType"/>
        /// and <see cref="CborAdditionalInfo"/>.
        /// </summary>
        /// <param name="majorType">The value of <see cref="MajorType"/>.</param>
        /// <param name="additionalInfo">The value of <see cref="AdditionalInfo"/>.</param>
        public CborInitialByte(CborMajorType majorType, CborAdditionalInfo additionalInfo)
        {
            Value = (byte)(((byte)majorType) | (byte)additionalInfo);
        }

        /// <summary>
        /// Constructs a new value using a single byte.
        /// </summary>
        /// <param name="initialByte">The value of <see cref="Value"/>.</param>
        public CborInitialByte(byte initialByte)
        {
            Value = initialByte;
        }
    }
}
