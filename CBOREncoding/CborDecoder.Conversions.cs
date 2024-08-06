using System;
using System.Formats.Cbor;
using System.Numerics;
using System.Runtime.CompilerServices;
using System.Text;

namespace IS4.Cbor
{
    using static CborReaderState;

    partial struct CborDecoder<TBuffer>
    {
        /// <summary>
        /// The current string value decoded as UTF-8.
        /// </summary>
        /// <exception cref="InvalidOperationException">
        /// <see cref="State"/> is not <see cref="TextString"/>
        /// or <see cref="CborReaderState.ByteString"/>.
        /// </exception>
        public readonly unsafe string StringValue {
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
                if(s.State == NegativeInteger)
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
                if(s.State == NegativeInteger)
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
                if(s.State == NegativeInteger)
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
                if(s.State == NegativeInteger)
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
                if(s.State == NegativeInteger)
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
                if(s.State == NegativeInteger)
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
                if(s.State == NegativeInteger)
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
                if(s.State == NegativeInteger)
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
                if(s.State == NegativeInteger)
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
                if(s.State == HalfPrecisionFloat)
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
                if(s.State == SinglePrecisionFloat)
                {
                    return Int32BitsToSingle((int)RawValue);
                }
                return (float)DoubleValue;
            }
        }

        /// <summary>
        /// The current atomic value as <see cref="Double"/>.
        /// </summary>
        public readonly double DoubleValue => s.State switch
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
                if(s.State != Tag)
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
                if(s.State != Boolean)
                {
                    throw new InvalidOperationException("The current value is not a boolean.");
                }
                return RawValue != 0;
            }
        }

        /// <summary>
        /// The current simple value.
        /// </summary>
        public readonly CborSimpleValue SimpleValue => s.State switch
        {
            Boolean => RawValue != 0 ? CborSimpleValue.True : CborSimpleValue.False,
            Null or CborReaderState.SimpleValue => (CborSimpleValue)RawValue,
            _ => throw new InvalidOperationException("The current value is not a simple value.")
        };

        /// <summary>
        /// The current atomic value, or <see langword="null"/>
        /// if no such value could be retrieved.
        /// </summary>
        public readonly object? ObjectValue => s.State switch
        {
            UnsignedInteger when s.ValueSize <= 1 => ByteValue,
            UnsignedInteger when s.ValueSize <= 2 => UInt16Value,
            UnsignedInteger when s.ValueSize <= 4 => UInt32Value,
            UnsignedInteger when s.ValueSize <= 8 => UInt64Value,
            UnsignedInteger => IntegerValue,
            NegativeInteger when s.ValueSize <= 1 => RawValue > (ulong)SByte.MaxValue ? Int16Value : SByteValue,
            NegativeInteger when s.ValueSize <= 2 => RawValue > (ulong)Int16.MaxValue ? Int32Value : Int16Value,
            NegativeInteger when s.ValueSize <= 4 => RawValue > Int32.MaxValue ? Int64Value : Int32Value,
            NegativeInteger when s.ValueSize <= 8 => RawValue > Int64.MaxValue ? IntegerValue : Int64Value,
            NegativeInteger => IntegerValue,
            HalfPrecisionFloat => HalfValue,
            SinglePrecisionFloat => SingleValue,
            DoublePrecisionFloat => DoubleValue,
            TextString => StringValue,
            CborReaderState.ByteString => ByteStringValue,
            Boolean => RawValue != 0,
            Null or CborReaderState.SimpleValue => (CborSimpleValue)RawValue,
            Tag => (CborTag)RawValue,
            _ => null
        };

        static float Int32BitsToSingle(int x) => Unsafe.As<int, float>(ref x);
    }
}
