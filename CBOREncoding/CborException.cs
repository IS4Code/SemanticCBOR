using System;
using System.Runtime.CompilerServices;
using System.Runtime.Serialization;

namespace IS4.Cbor
{
    /// <summary>
    /// An exception type thrown in any situation related to CBOR data manipulation.
    /// </summary>
    public abstract class CborException : Exception
    {
        /// <inheritdoc cref="Exception.Exception(string)"/>
        public CborException(string message) : base(message)
        {

        }

        protected CborException(SerializationInfo info, StreamingContext context) : base(info, context)
        {

        }
    }

    /// <summary>
    /// An exception type thrown when new data needs to be provided to <see cref="CborDecoder{TBuffer}"/>.
    /// </summary>
    /// <typeparam name="TBuffer"><inheritdoc cref="CborDecoderCheckpoint{TBuffer}{TBuffer}" path="/typeparam[@name='TBuffer']"/></typeparam>
    public sealed class CborCheckpointException<TBuffer> : CborException where TBuffer : unmanaged
    {
        readonly CborDecoderCheckpoint<TBuffer> checkpoint;

        /// <summary>
        /// The checkpoint stored by the previous <see cref="CborDecoder{TBuffer}"/>
        /// instance that needs to be used to initialize the next one.
        /// </summary>
        public ref readonly CborDecoderCheckpoint<TBuffer> Checkpoint {
            [MethodImpl(MethodImplOptions.AggressiveInlining)]
            get => ref checkpoint;
        }

        internal CborCheckpointException(string message, ref CborDecoder<TBuffer> decoder) : base(message)
        {
            decoder.StoreToCheckpoint(out checkpoint);
        }

        private CborCheckpointException(SerializationInfo info, StreamingContext context) : base(info, context)
        {

        }
    }

    /// <summary>
    /// An exception type thrown when invalid CBOR content is consumed or produced.
    /// </summary>
    public class CborContentException : CborException
    {
        /// <inheritdoc cref="CborException.CborException(string)"/>
        public CborContentException(string message) : base(message)
        {

        }

        private CborContentException(SerializationInfo info, StreamingContext context) : base(info, context)
        {

        }
    }
}
