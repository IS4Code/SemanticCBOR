using System.Formats.Cbor;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

namespace IS4.Cbor
{
    partial struct CborDecoder<TBuffer>
    {
        /// <summary>
        /// An opaque state produced by <see cref="CborDecoder"/> to share
        /// across chunks of the input data stream.
        /// </summary>
        [StructLayout(LayoutKind.Auto)]
        public struct Checkpoint
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
    }
}
