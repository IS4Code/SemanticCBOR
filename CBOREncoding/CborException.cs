using System;
using System.Runtime.CompilerServices;
using System.Runtime.Serialization;

namespace IS4.Cbor
{
    public abstract class CborException : Exception
    {
        public CborException(string message) : base(message)
        {

        }

        protected CborException(SerializationInfo info, StreamingContext context) : base(info, context)
        {

        }
    }

    public sealed class CborCheckpointException : CborException
    {
        readonly CborDecoderCheckpoint checkpoint;

        public ref readonly CborDecoderCheckpoint Checkpoint {
            [MethodImpl(MethodImplOptions.AggressiveInlining)]
            get => ref checkpoint;
        }

        internal CborCheckpointException(string message, ref CborDecoder decoder) : base(message)
        {
            decoder.StoreToCheckpoint(out checkpoint);
        }

        private CborCheckpointException(SerializationInfo info, StreamingContext context) : base(info, context)
        {

        }
    }

    public class CborContentException : CborException
    {
        public CborContentException(string message) : base(message)
        {

        }

        private CborContentException(SerializationInfo info, StreamingContext context) : base(info, context)
        {

        }
    }
}
