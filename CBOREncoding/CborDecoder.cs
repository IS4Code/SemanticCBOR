using System;
using System.Buffers.Binary;
using System.Diagnostics;
using System.Diagnostics.CodeAnalysis;
using System.Formats.Cbor;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

namespace IS4.Cbor
{
    [StructLayout(LayoutKind.Auto)]
    public ref struct CborDecoder
    {
        public const CborReaderState EndIndefiniteLengthCollection = (CborReaderState)254;

        public const int MinNonFinalChunkLength = CborDecoderCheckpoint.CacheMaxSize;

        CborDecoderCheckpoint s;
        readonly ReadOnlySpan<byte> currentChunk;
        readonly bool isFinal;
        int offset;

        readonly unsafe Span<byte> cache {
            [UnscopedRef]
            get {
                // This is a ref struct so the fields are already fixed
                fixed(byte* ptr = s.Cache)
                {
                    Debug.Assert(s.CacheSize <= CborDecoderCheckpoint.CacheMaxSize);
                    return new Span<byte>(ptr, s.CacheSize);
                }
            }
        }

        readonly ReadOnlySpan<byte> chunkView {
            [MethodImpl(MethodImplOptions.AggressiveInlining)]
            get => currentChunk[offset..];
        }

        readonly ReadOnlySpan<byte> cacheView {
            [UnscopedRef, MethodImpl(MethodImplOptions.AggressiveInlining)]
            get => cache[^-offset..];
        }

        readonly ReadOnlySpan<byte> dataView {
            [UnscopedRef, MethodImpl(MethodImplOptions.AggressiveInlining)]
            get => offset >= 0 ? chunkView : cacheView;
        }

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

        public readonly CborReaderState State {
            [MethodImpl(MethodImplOptions.AggressiveInlining)]
            get => s.State;
        }

        public readonly CborInitialByte InitialByte {
            [MethodImpl(MethodImplOptions.AggressiveInlining)]
            get => s.InitialByte;
        }

        public readonly int RawSize {
            [MethodImpl(MethodImplOptions.AggressiveInlining)]
            get => s.ValueSize;
        }

        public readonly ulong RawValue {
            [MethodImpl(MethodImplOptions.AggressiveInlining)]
            get => s.RawValue;
        }

        public readonly bool IsIndefiniteLength {
            [MethodImpl(MethodImplOptions.AggressiveInlining)]
            get => s.InitialByte.AdditionalInfo == CborAdditionalInfo.IndefiniteLength;
        }

        public CborDecoder(ReadOnlySpan<byte> initialChunk, bool isFinalChunk)
        {
            s = default;
            currentChunk = initialChunk;
            isFinal = isFinalChunk;
            offset = 0;

            Initialize(nameof(initialChunk));
        }

        public CborDecoder(scoped in CborDecoderCheckpoint previousCheckpoint, ReadOnlySpan<byte> nextChunk, bool isFinalChunk)
        {
            s = previousCheckpoint;
            currentChunk = nextChunk;
            isFinal = isFinalChunk;
            offset = -s.CacheSize;

            Initialize(nameof(nextChunk));
        }

        private void Initialize(string paramName)
        {
            if(!HasEnoughSpace)
            {
                throw new ArgumentException($"Non-final input data chunk must have at least {MinNonFinalChunkLength} bytes.", paramName);
            }
            Decode();
        }

        public CborReaderState PeekState()
        {
            Decode();
            return s.State;
        }

        readonly bool HasEnoughSpace {
            [MethodImpl(MethodImplOptions.AggressiveInlining)]
            get => isFinal || currentChunk.Length - offset >= MinNonFinalChunkLength;
        }

        readonly bool IsChunkedString {
            [MethodImpl(MethodImplOptions.AggressiveInlining)]
            get => s.StringChunk is not StringChunkState.None;
        }

        readonly bool ShouldDecodeAfterStringChunk {
            [MethodImpl(MethodImplOptions.AggressiveInlining)]
            get => IsChunkedString && s.ValueSize == 0;
        }

        readonly bool ShouldCallDecode {
            [MethodImpl(MethodImplOptions.AggressiveInlining)]
            get => s.State == CborReaderState.Undefined || ShouldDecodeAfterStringChunk;
        }

        void EnsureEnoughSpace()
        {
            if(!HasEnoughSpace)
            {
                throw NeedsMoreData();
            }
        }

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
                        if(s.StringType == 0)
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
            }
            s.State = CborReaderState.Undefined;
        }

        public bool TryGetCheckpoint(out CborDecoderCheckpoint checkpoint)
        {
            if(HasEnoughSpace)
            {
                // Not needed
                Unsafe.SkipInit(out checkpoint);
                return false;
            }

            StoreToCheckpoint(out checkpoint);
            return true;
        }

        internal void StoreToCheckpoint(out CborDecoderCheckpoint checkpoint)
        {
            // Copy remaining into cache
            var remaining = chunkView;
            if(remaining.Length > CborDecoderCheckpoint.CacheMaxSize)
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
            offset = currentChunk.Length;
        }

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

            if(s.StringType != 0 && s.InitialByte.Value != 0xFF && s.InitialByte.MajorType != s.StringType)
            {
                throw new CborContentException($"Encountered wrong indefinite-length string chunk type ({s.InitialByte.MajorType} but {s.StringType} required).");
            }

            switch(s.State)
            {
                case CborReaderState.TextString:
                case CborReaderState.ByteString:
                    if(s.ValueSize == -1)
                    {
                        // Indefinite length
                        if(s.StringType != 0)
                        {
                            // Nested
                            throw new CborContentException($"Indefinite-length {s.State} encountered inside another.");
                        }
                        // Store type and move to the Start state
                        s.StringType = s.InitialByte.MajorType;
                        s.State++;
                        return;
                    }
                    // Finite length but maybe split over multiple spans
                    if(!ProcessStringChunk(StringChunkState.None))
                    {
                        Debug.Assert(IsChunkedString);
                        // Simulate indefinite length if not already
                        if(s.StringType == 0)
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

            if(s.ValueSize == -1)
            {
                if(s.State != CborReaderState.SimpleValue)
                {
                    throw new CborContentException($"Invalid value byte 0x{s.InitialByte.Value:X2} (indefinite length specified for atomic value).");
                }
                // Break code
                switch(s.StringType)
                {
                    case CborMajorType.TextString:
                        s.State = CborReaderState.EndIndefiniteLengthTextString;
                        return;
                    case CborMajorType.ByteString:
                        s.State = CborReaderState.EndIndefiniteLengthByteString;
                        return;
                    default:
                        s.State = EndIndefiniteLengthCollection;
                        return;
                }
            }

            if(s.State != CborReaderState.SimpleValue)
            {
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
            if(s.RawValue <= CborDecoderCheckpoint.CacheMaxSize && unchecked(offset + (int)s.RawValue <= 0))
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

        readonly CborContentException UnexpectedEnd()
        {
            return new CborContentException($"Unexpected end of data for {s.State}.");
        }

        CborCheckpointException NeedsMoreData()
        {
            return new CborCheckpointException("A new chunk needs to be provided to continue reading using the reader's checkpoint.", ref this);
        }
    }

    [StructLayout(LayoutKind.Auto)]
    public struct CborDecoderCheckpoint
    {
        // Must be enough to read a single value (initial byte + argument)
        internal const int CacheMaxSize = sizeof(byte) + sizeof(ulong);

        byte _state;
        internal CborReaderState State {
            [MethodImpl(MethodImplOptions.AggressiveInlining)]
            readonly get => (CborReaderState)_state;
            [MethodImpl(MethodImplOptions.AggressiveInlining)]
            set => _state = checked((byte)value);
        }

        internal CborInitialByte InitialByte;
        internal StringChunkState StringChunk;
        internal CborMajorType StringType;
        internal int ValueSize;
        internal ulong RawValue;

        internal int CacheSize;
        internal unsafe fixed byte Cache[CacheMaxSize];
    }

    enum StringChunkState : byte
    {
        None = 0,
        Over1 = 1,
        Over2 = 2,
        Over3 = 3,

        Contiguous = 10
    }

    public enum CborMajorType : byte
    {
        UnsignedInteger = 0 << CborInitialByte.MajorTypeShift,
        NegativeInteger = 1 << CborInitialByte.MajorTypeShift,
        ByteString = 2 << CborInitialByte.MajorTypeShift,
        TextString = 3 << CborInitialByte.MajorTypeShift,
        Array = 4 << CborInitialByte.MajorTypeShift,
        Map = 5 << CborInitialByte.MajorTypeShift,
        Tag = 6 << CborInitialByte.MajorTypeShift,
        Simple = 7 << CborInitialByte.MajorTypeShift,
    }

    public enum CborAdditionalInfo : byte
    {
        Additional8BitData = 24,
        Additional16BitData = 25,
        Additional32BitData = 26,
        Additional64BitData = 27,
        IndefiniteLength = 31,
    }

    [StructLayout(LayoutKind.Sequential, Pack = 1, Size = 1)]
    public readonly struct CborInitialByte
    {
        public const byte IndefiniteLengthBreakByte = 0xff;
        public const byte AdditionalInformationMask = 0b000_11111;
        public const int MajorTypeShift = 5;

        public byte Value { get; }

        public CborInitialByte(CborMajorType majorType, CborAdditionalInfo additionalInfo)
        {
            Value = (byte)(((byte)majorType) | (byte)additionalInfo);
        }

        public CborInitialByte(byte initialByte)
        {
            Value = initialByte;
        }

        public CborMajorType MajorType => (CborMajorType)(Value &~ AdditionalInformationMask);
        public CborAdditionalInfo AdditionalInfo => (CborAdditionalInfo)(Value & AdditionalInformationMask);
    }
}
