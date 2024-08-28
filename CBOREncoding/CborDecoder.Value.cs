using System;
using System.Diagnostics;
using System.Diagnostics.CodeAnalysis;
using System.Formats.Cbor;
using System.Numerics;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;
using System.Text;

namespace IS4.Cbor
{
    using static CborReaderState;

    /// <summary>
    /// Supports receiving a value of an arbitrary type.
    /// </summary>
    /// <typeparam name="TArgs">Additional arguments given to the receiver.</typeparam>
    /// <typeparam name="TResult">The result produced by the receiver.</typeparam>
    public interface IGenericReceiver<in TArgs, out TResult>
    {
        /// <summary>
        /// Invokes the receiver with the value and additional arguments.
        /// </summary>
        /// <typeparam name="TValue">The type of <paramref name="value"/>.</typeparam>
        /// <param name="value">The value given to the receiver.</param>
        /// <param name="args">Additional arguments to the receiver.</param>
        /// <returns>The value produced by the receiver.</returns>
        TResult Invoke<TValue>(TValue value, TArgs args);
    }

    partial struct CborDecoder<TBuffer>
    {
        /// <summary>
        /// Stores information about a value obtained as a result of CBOR decoding.
        /// </summary>
        [StructLayout(LayoutKind.Auto)]
        public ref partial struct Value
        {
            /// <summary>
            /// The current state of the decoder.
            /// </summary>
            internal Checkpoint Checkpoint;

            /// <summary>
            /// The current chunk of data processed by the decoder.
            /// </summary>
            internal readonly ReadOnlySpan<byte> CurrentChunk;

            /// <summary>
            /// Initializes a new CBOR value.
            /// </summary>
            /// <param name="preserveNestedCollections">Whether information about nested collections is preserved.</param>
            /// <param name="currentChunk">The current data span.</param>
            internal Value(bool preserveNestedCollections, ReadOnlySpan<byte> currentChunk)
            {
                Checkpoint = default;
                if(preserveNestedCollections)
                {
                    Checkpoint.CollectionStates = new();
                }
                Checkpoint.CollectionState.NextIsValue = true;
                CurrentChunk = currentChunk;
            }

            /// <summary>
            /// Initializes a CBOR value from checkpoint.
            /// </summary>
            /// <param name="previousCheckpoint">The previously stored checkpoint.</param>
            /// <param name="currentChunk">The current data span.</param>
            internal Value(scoped in Checkpoint previousCheckpoint, ReadOnlySpan<byte> currentChunk)
            {
                Checkpoint = previousCheckpoint;
                CurrentChunk = currentChunk;
            }

            /// <summary>
            /// The current position in the currently decoded chunk.
            /// May be negative, in which case it points into the cached bytes from the previous chunk.
            /// </summary>
            public int Offset { get; internal set; }

            /// <summary>
            /// Accesses the previously cached data.
            /// </summary>
            internal readonly unsafe Span<byte> Cache {
                [UnscopedRef]
                get {
                    // This is a ref struct so the fields are already fixed
                    fixed(void* ptr = &Checkpoint.Cache)
                    {
                        Debug.Assert(Checkpoint.CacheSize <= Checkpoint.CacheMaxSize);
                        return new Span<byte>(ptr, Checkpoint.CacheSize);
                    }
                }
            }

            /// <summary>
            /// Accesses the data in <see cref="CurrentChunk"/> beginning at <see cref="Offset"/>.
            /// </summary>
            internal readonly ReadOnlySpan<byte> ChunkView {
                [MethodImpl(MethodImplOptions.AggressiveInlining)]
                get => CurrentChunk[Offset..];
            }

            /// <summary>
            /// Accesses the data in <see cref="Cache"/> beginning at <see cref="Offset"/>.
            /// </summary>
            internal readonly ReadOnlySpan<byte> CacheView {
                [UnscopedRef, MethodImpl(MethodImplOptions.AggressiveInlining)]
                get => Cache[^-Offset..];
            }

            /// <summary>
            /// Obtains either <see cref="ChunkView"/> or <see cref="CacheView"/> based on
            /// the sign of <see cref="Offset"/>.
            /// </summary>
            internal readonly ReadOnlySpan<byte> DataView {
                [UnscopedRef, MethodImpl(MethodImplOptions.AggressiveInlining)]
                get => Offset >= 0 ? ChunkView : CacheView;
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
                    switch(Checkpoint.State)
                    {
                        case CborReaderState.ByteString:
                        case CborReaderState.TextString:
                            break;
                        default:
                            throw new InvalidOperationException($"Value bytes are exposed only for immediate string values (current state is {Checkpoint.State}).");
                    }
                    // Never set to overlap both spans
                    return DataView.Slice(0, Checkpoint.ValueSize);
                }
            }
        
            /// <summary>
            /// The current state of the decoder.
            /// </summary>
            public readonly CborReaderState State {
                [MethodImpl(MethodImplOptions.AggressiveInlining)]
                get => Checkpoint.State;
            }

            /// <summary>
            /// The initial byte header of the current value.
            /// </summary>
            public readonly CborInitialByte InitialByte {
                [MethodImpl(MethodImplOptions.AggressiveInlining)]
                get => Checkpoint.InitialByte;
            }

            /// <summary>
            /// Retrieves the raw size of the value. If <see cref="ValueBytes"/>
            /// is available, retrieves its length. In other states that have a value,
            /// retrieves the size of <see cref="RawValue"/>.
            /// </summary>
            public readonly int RawSize {
                [MethodImpl(MethodImplOptions.AggressiveInlining)]
                get => Checkpoint.ValueSize;
            }

            /// <summary>
            /// The atomic value corresponding to the current state.
            /// For definite-length collections, this value stores the number
            /// of the elements in the collection.
            /// </summary>
            public readonly ulong RawValue {
                [MethodImpl(MethodImplOptions.AggressiveInlining)]
                get => Checkpoint.RawValue;
            }

            /// <summary>
            /// Whether the current value is marked as having indefinite length in the data.
            /// </summary>
            public readonly bool IsIndefiniteLength {
                [MethodImpl(MethodImplOptions.AggressiveInlining)]
                get => Checkpoint.InitialByte.AdditionalInfo == CborAdditionalInfo.IndefiniteLength;
            }

            /// <summary>
            /// The length of the current sequence value.
            /// </summary>
            public readonly ulong? ValueLength {
                [MethodImpl(MethodImplOptions.AggressiveInlining)]
                get => IsIndefiniteLength ? null : RawValue;
            }

            /// <summary>
            /// Whether information about nested collections is preserved.
            /// </summary>
            public readonly bool PreservesNestedCollections {
                [MethodImpl(MethodImplOptions.AggressiveInlining)]
                get => Checkpoint.CollectionStates.IsInitialized;
            }

            /// <summary>
            /// The current depth of nested collections, if <see cref="PreservesNestedCollections"/>
            /// is <see langword="true"/>.
            /// </summary>
            public readonly int CurrentDepth {
                [MethodImpl(MethodImplOptions.AggressiveInlining)]
                get => Checkpoint.CollectionStates.Count;
            }

            /// <summary>
            /// Whether the current value is a key in a map.
            /// </summary>
            public readonly bool IsMapKey {
                [MethodImpl(MethodImplOptions.AggressiveInlining)]
                get => !Checkpoint.CollectionState.NextIsValue;
            }

            /// <summary>
            /// The number of remaining elements in the current collection, if available.
            /// </summary>
            public readonly ulong? RemainingCollectionElements {
                [MethodImpl(MethodImplOptions.AggressiveInlining)]
                get => PreservesNestedCollections && Checkpoint.CollectionState.CollectionByte.AdditionalInfo != CborAdditionalInfo.IndefiniteLength
                    ? Checkpoint.CollectionState.RemainingElements
                    : null;
            }

            /// <summary>
            /// The current string value decoded as UTF-8.
            /// </summary>
            /// <exception cref="InvalidOperationException">
            /// <see cref="State"/> is not <see cref="TextString"/>
            /// or <see cref="CborReaderState.ByteString"/>.
            /// </exception>
            public readonly unsafe string TextStringValue {
                get {
                    var bytes = ValueBytes;
                    if(bytes.Length == 0)
                    {
                        return "";
                    }
                    fixed(byte* ptr = bytes)
                    {
                        return Encoding.UTF8.GetString(ptr, bytes.Length);
                    }
                }
            }

            /// <summary>
            /// The current string value as an <see cref="ByteString"/> instance.
            /// </summary>
            /// <exception cref="InvalidOperationException">
            /// <see cref="State"/> is not <see cref="TextString"/>
            /// or <see cref="CborReaderState.ByteString"/>.
            /// </exception>
            public readonly ByteString ByteStringValue => new(ValueBytes);

            const string negativeOverflow = "Negative integers cannot be returned via an unsigned integer type.";

            /// <summary>
            /// The current atomic value as <see cref="Byte"/>.
            /// </summary>
            /// <exception cref="OverflowException">
            /// The value exceeds the integral type.
            /// </exception>
            public readonly byte ByteValue {
                get {
                    if(Checkpoint.State == NegativeInteger)
                    {
                        throw new OverflowException(negativeOverflow);
                    }
                    return checked((byte)RawValue);
                }
            }

            /// <summary>
            /// The current atomic value as <see cref="UInt16"/>.
            /// </summary>
            /// <exception cref="OverflowException">
            /// The value exceeds the integral type.
            /// </exception>
            public readonly ushort UInt16Value {
                get {
                    if(Checkpoint.State == NegativeInteger)
                    {
                        throw new OverflowException(negativeOverflow);
                    }
                    return checked((ushort)RawValue);
                }
            }

            /// <summary>
            /// The current atomic value as <see cref="UInt32"/>.
            /// </summary>
            /// <exception cref="OverflowException">
            /// The value exceeds the integral type.
            /// </exception>
            public readonly uint UInt32Value {
                get {
                    if(Checkpoint.State == NegativeInteger)
                    {
                        throw new OverflowException(negativeOverflow);
                    }
                    return checked((uint)RawValue);
                }
            }

            /// <summary>
            /// The current atomic value as <see cref="UInt64"/>.
            /// </summary>
            /// <exception cref="OverflowException">
            /// The value exceeds the integral type.
            /// </exception>
            public readonly ulong UInt64Value {
                get {
                    if(Checkpoint.State == NegativeInteger)
                    {
                        throw new OverflowException(negativeOverflow);
                    }
                    return RawValue;
                }
            }

            /// <summary>
            /// The current atomic value as <see cref="SByte"/>.
            /// </summary>
            /// <exception cref="OverflowException">
            /// The value exceeds the integral type.
            /// </exception>
            public readonly sbyte SByteValue {
                get {
                    if(Checkpoint.State == NegativeInteger)
                    {
                        return checked((sbyte)(-1 - (int)RawValue));
                    }
                    return checked((sbyte)RawValue);
                }
            }

            /// <summary>
            /// The current atomic value as <see cref="Int16"/>.
            /// </summary>
            /// <exception cref="OverflowException">
            /// The value exceeds the integral type.
            /// </exception>
            public readonly short Int16Value {
                get {
                    if(Checkpoint.State == NegativeInteger)
                    {
                        return checked((short)(-1 - (int)RawValue));
                    }
                    return checked((short)RawValue);
                }
            }

            /// <summary>
            /// The current atomic value as <see cref="Int32"/>.
            /// </summary>
            /// <exception cref="OverflowException">
            /// The value exceeds the integral type.
            /// </exception>
            public readonly int Int32Value {
                get {
                    if(Checkpoint.State == NegativeInteger)
                    {
                        return checked(-1 - (int)RawValue);
                    }
                    return checked((int)RawValue);
                }
            }

            /// <summary>
            /// The current atomic value as <see cref="Int64"/>.
            /// </summary>
            /// <exception cref="OverflowException">
            /// The value exceeds the integral type.
            /// </exception>
            public readonly long Int64Value {
                get {
                    if(Checkpoint.State == NegativeInteger)
                    {
                        return checked(-1L - (long)RawValue);
                    }
                    return checked((long)RawValue);
                }
            }

            /// <summary>
            /// The current atomic value as <see cref="BigInteger"/>.
            /// </summary>
            public readonly BigInteger IntegerValue {
                get {
                    if(Checkpoint.State == NegativeInteger)
                    {
                        return BigInteger.MinusOne - new BigInteger(RawValue);
                    }
                    return new BigInteger(RawValue);
                }
            }

            /// <summary>
            /// The current atomic value as <see cref="Half"/>.
            /// </summary>
            public readonly Half HalfValue {
                get {
                    if(Checkpoint.State == HalfPrecisionFloat)
                    {
                        return Half.ToHalf((ushort)RawValue);
                    }
                    return (Half)DoubleValue;
                }
            }

            /// <summary>
            /// The current atomic value as <see cref="Single"/>.
            /// </summary>
            public readonly float SingleValue {
                get {
                    if(Checkpoint.State == SinglePrecisionFloat)
                    {
                        return Int32BitsToSingle((int)RawValue);
                    }
                    return (float)DoubleValue;
                }
            }

            /// <summary>
            /// The current atomic value as <see cref="Double"/>.
            /// </summary>
            public readonly double DoubleValue => Checkpoint.State switch
            {
                HalfPrecisionFloat => HalfValue,
                SinglePrecisionFloat => SingleValue,
                DoublePrecisionFloat => BitConverter.Int64BitsToDouble(unchecked((long)RawValue)),
                UnsignedInteger => RawValue,
                NegativeInteger => -1D - RawValue,
                _ => throw new InvalidOperationException("The current value is not an atomic number.")
            };

            /// <summary>
            /// The current tag value.
            /// </summary>
            public readonly CborTag TagValue {
                get {
                    if(Checkpoint.State != Tag)
                    {
                        throw new InvalidOperationException("The current value is not a tag.");
                    }
                    return (CborTag)RawValue;
                }
            }

            /// <summary>
            /// The current boolean value.
            /// </summary>
            public readonly bool BooleanValue {
                get {
                    if(Checkpoint.State != Boolean)
                    {
                        throw new InvalidOperationException("The current value is not a boolean.");
                    }
                    return RawValue != 0;
                }
            }

            /// <summary>
            /// The current simple value.
            /// </summary>
            public readonly CborSimpleValue SimpleValue => Checkpoint.State switch
            {
                Boolean => RawValue != 0 ? CborSimpleValue.True : CborSimpleValue.False,
                Null or CborReaderState.SimpleValue => (CborSimpleValue)RawValue,
                _ => throw new InvalidOperationException("The current value is not a simple value.")
            };

            [MethodImpl(MethodImplOptions.AggressiveInlining)]
            private readonly TResult RetrieveValueCore<TArgs, TResult, TReceiver>(ref TReceiver receiver, TArgs args) where TReceiver : IGenericReceiver<TArgs, TResult>
            {
                return Checkpoint.State switch
                {
                    UnsignedInteger when Checkpoint.ValueSize <= 1 => receiver.Invoke(ByteValue, args),
                    UnsignedInteger when Checkpoint.ValueSize <= 2 => receiver.Invoke(UInt16Value, args),
                    UnsignedInteger when Checkpoint.ValueSize <= 4 => receiver.Invoke(UInt32Value, args),
                    UnsignedInteger when Checkpoint.ValueSize <= 8 => receiver.Invoke(UInt64Value, args),
                    UnsignedInteger => receiver.Invoke(IntegerValue, args),
                    NegativeInteger when Checkpoint.ValueSize <= 1 => RawValue > (ulong)SByte.MaxValue ? receiver.Invoke(Int16Value, args) : receiver.Invoke(SByteValue, args),
                    NegativeInteger when Checkpoint.ValueSize <= 2 => RawValue > (ulong)Int16.MaxValue ? receiver.Invoke(Int32Value, args) : receiver.Invoke(Int16Value, args),
                    NegativeInteger when Checkpoint.ValueSize <= 4 => RawValue > Int32.MaxValue ? receiver.Invoke(Int64Value, args) : receiver.Invoke(Int32Value, args),
                    NegativeInteger when Checkpoint.ValueSize <= 8 => RawValue > Int64.MaxValue ? receiver.Invoke(IntegerValue, args) : receiver.Invoke(Int64Value, args),
                    NegativeInteger => receiver.Invoke(IntegerValue, args),
                    HalfPrecisionFloat => receiver.Invoke(HalfValue, args),
                    SinglePrecisionFloat => receiver.Invoke(SingleValue, args),
                    DoublePrecisionFloat => receiver.Invoke(DoubleValue, args),
                    TextString => receiver.Invoke(TextStringValue, args),
                    CborReaderState.ByteString => receiver.Invoke(ByteStringValue, args),
                    Boolean => receiver.Invoke(RawValue != 0, args),
                    Null or CborReaderState.SimpleValue => receiver.Invoke((CborSimpleValue)RawValue, args),
                    Tag => receiver.Invoke((CborTag)RawValue, args),
                    _ => receiver.Invoke<object?>(null, args)
                };
            }

            /// <summary>
            /// Retrieves a typed value corresponding to the current atomic value,
            /// through an instance of <see cref="IGenericReceiver{TArgs, TResult}"/>.
            /// </summary>
            /// <typeparam name="TArgs">The type of <paramref name="args"/>.</typeparam>
            /// <typeparam name="TResult">The result of invoking <paramref name="receiver"/>.</typeparam>
            /// <typeparam name="TReceiver">The type of <paramref name="receiver"/>.</typeparam>
            /// <param name="receiver">The instance to receive the value.</param>
            /// <param name="args">The arguments given to <paramref name="receiver"/>.</param>
            /// <returns>The result of invoking <paramref name="receiver"/>.</returns>
            public readonly TResult RetrieveValue<TArgs, TResult, TReceiver>(ref TReceiver receiver, TArgs args) where TReceiver : struct, IGenericReceiver<TArgs, TResult>
            {
                return RetrieveValueCore<TArgs, TResult, TReceiver>(ref receiver, args);
            }

            /// <inheritdoc cref="RetrieveValue{TArgs, TResult, TReceiver}(ref TReceiver, TArgs)"/>
            public readonly TResult RetrieveValue<TArgs, TResult, TReceiver>(TReceiver receiver, TArgs args) where TReceiver : IGenericReceiver<TArgs, TResult>
            {
                return RetrieveValueCore<TArgs, TResult, TReceiver>(ref receiver, args);
            }

            /// <inheritdoc cref="RetrieveValue{TArgs, TResult, TReceiver}(ref TReceiver, TArgs)"/>
            public readonly TResult RetrieveValue<TArgs, TResult>(IGenericReceiver<TArgs, TResult> receiver, TArgs args)
            {
                return RetrieveValueCore<TArgs, TResult, IGenericReceiver<TArgs, TResult>>(ref receiver, args);
            }

            /// <summary>
            /// The current atomic value, or <see langword="null"/>
            /// if no such value could be retrieved.
            /// </summary>
            public readonly object? ObjectValue => RetrieveValue<ValueTuple, object?, BoxingReceiver>(ref BoxingReceiver.Instance, default);

            readonly struct BoxingReceiver : IGenericReceiver<ValueTuple, object?>
            {
                public static BoxingReceiver Instance = new();

                public object? Invoke<TValue>(TValue value, ValueTuple args)
                {
                    return value;
                }
            }

            static float Int32BitsToSingle(int x) => Unsafe.As<int, float>(ref x);
        }
    }
}
