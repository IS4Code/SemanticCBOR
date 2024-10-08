﻿using System;
using System.Collections;
using System.Collections.Generic;
using System.Diagnostics.CodeAnalysis;
using System.IO;
using System.Runtime.InteropServices;
using System.Text;
using System.Threading;
using System.Threading.Tasks;

namespace IS4.Cbor
{
    /// <summary>
    /// An immutable string of bytes.
    /// </summary>
    public readonly struct ByteString : IEquatable<ByteString>, IReadOnlyList<byte>, ICloneable
    {
        readonly byte[]? _data;
        internal byte[] Data => _data ?? Array.Empty<byte>();

        /// <summary>
        /// The length of the string.
        /// </summary>
        public int Length => _data?.Length ?? 0;

        int IReadOnlyCollection<byte>.Count => Length;

        /// <summary>
        /// Accesses a particular byte of the string.
        /// </summary>
        /// <param name="index">The index of the byte.</param>
        /// <returns>The byte at <paramref name="index"/>.</returns>
        public byte this[int index] => Data[index];

        /// <summary>
        /// Constructs a new instance by value.
        /// </summary>
        /// <param name="data">The data to use to construct the value.</param>
        public ByteString(ReadOnlySpan<byte> data)
        {
            _data = data.ToArray();
        }

        private ByteString(byte[] data)
        {
            _data = data;
        }

        /// <inheritdoc cref="IEquatable{T}.Equals(T)"/>
        public bool Equals(ByteString other)
        {
            return _data == other._data || Data.AsSpan().SequenceEqual(other.Data.AsSpan());
        }

        /// <inheritdoc/>
        public override bool Equals([NotNullWhen(true)] object? obj)
        {
            return obj is ByteString v && Equals(v);
        }

        /// <summary>
        /// Compares two byte strings for equality.
        /// </summary>
        /// <param name="a">The first string to compare.</param>
        /// <param name="b">The second string to compare.</param>
        /// <returns>Whether <paramref name="a"/> and <paramref name="b"/> are equal.</returns>
        public static bool operator ==(ByteString a, ByteString b)
        {
            return a.Equals(b);
        }

        /// <summary>
        /// Compares two byte strings for inequality.
        /// </summary>
        /// <param name="a">The first string to compare.</param>
        /// <param name="b">The second string to compare.</param>
        /// <returns>Whether <paramref name="a"/> and <paramref name="b"/> are not equal.</returns>
        public static bool operator !=(ByteString a, ByteString b)
        {
            return !a.Equals(b);
        }

        /// <summary>
        /// Obtains a substring of the string.
        /// </summary>
        /// <param name="startIndex">The index where the substring starts.</param>
        /// <returns>A substring formed from the bytes starting at <paramref name="startIndex"/>.</returns>
        public ByteString Substring(int startIndex)
        {
            if(startIndex == 0)
            {
                return this;
            }
            return new(Data.AsSpan().Slice(startIndex));
        }

        /// <summary>
        /// Obtains a substring of the string.
        /// </summary>
        /// <param name="startIndex">The index where the substring starts.</param>
        /// <param name="length">The length of the resulting string.</param>
        /// <returns>A substring formed from the bytes starting at <paramref name="startIndex"/>.</returns>
        public ByteString Substring(int startIndex, int length)
        {
            if(startIndex == 0 && length == Length)
            {
                return this;
            }
            return new(Data.AsSpan().Slice(startIndex, length));
        }

        /// <summary>
        /// Concatenates two byte strings.
        /// </summary>
        /// <param name="a">The first string to concatenate.</param>
        /// <param name="b">The second string to concatenate.</param>
        /// <returns>The result of the concatenation.</returns>
        public static ByteString operator +(ByteString a, ByteString b)
        {
            if(b.Length == 0)
            {
                return a;
            }
            if(a.Length == 0)
            {
                return b;
            }
            var array = a.Data;
            Array.Resize(ref array, array.Length + b.Length);
            b.AsSpan().CopyTo(array.AsSpan(a.Length));
            return new(array);
        }

        /// <inheritdoc/>
        public override int GetHashCode()
        {
            var span = Data.AsSpan();
            HashCode hashCode = default;
            while(span.Length >= 8)
            {
                hashCode.Add(MemoryMarshal.Read<long>(span));
                span = span[8..];
            }
            while(span.Length >= 4)
            {
                hashCode.Add(MemoryMarshal.Read<int>(span));
                span = span[4..];
            }
            while(span.Length >= 2)
            {
                hashCode.Add(MemoryMarshal.Read<short>(span));
                span = span[2..];
            }
            if(span.Length >= 1)
            {
                hashCode.Add(span[0]);
            }
            return hashCode.ToHashCode();
        }

        /// <inheritdoc/>
        public override string ToString()
        {
            return BitConverter.ToString(Data);
        }

        object ICloneable.Clone()
        {
            return this;
        }

        /// <summary>
        /// Obtains a <see cref="ReadOnlyMemory{T}"/> pointing to the string data.
        /// </summary>
        /// <returns>A memory range of the bytes.</returns>
        public ReadOnlyMemory<byte> AsMemory()
        {
            return Data.AsMemory();
        }

        /// <summary>
        /// Obtains a <see cref="ReadOnlySpan{T}"/> pointing to the string data.
        /// </summary>
        /// <returns>A span pointing to the bytes in memory.</returns>
        public ReadOnlySpan<byte> AsSpan()
        {
            return Data.AsSpan();
        }

        /// <inheritdoc cref="IEnumerable{T}.GetEnumerator"/>
        public IEnumerator<byte> GetEnumerator()
        {
            return ((IEnumerable<byte>)Data).GetEnumerator();
        }

        IEnumerator IEnumerable.GetEnumerator()
        {
            return GetEnumerator();
        }
    }

    /// <summary>
    /// Provides extensions for using <see cref="ByteString"/> with other types.
    /// </summary>
    public static class ByteStringOverloadExtensions
    {
        /// <inheritdoc cref="Stream.Write(byte[], int, int)"/>
        /// <param name="stream">The instance of <see cref="Stream"/> to write to.</param>
        /// <param name="byteString">The byte string to write to <paramref name="stream"/>.</param>
        public static void Write(this Stream stream, ByteString byteString)
        {
            stream.Write(byteString.Data, 0, byteString.Length);
        }

        /// <inheritdoc cref="Stream.WriteAsync(byte[], int, int)"/>
        /// <param name="stream">The instance of <see cref="Stream"/> to write to.</param>
        /// <param name="byteString">The byte string to write to <paramref name="stream"/>.</param>
        public static Task WriteAsync(this Stream stream, ByteString byteString)
        {
            return stream.WriteAsync(byteString.Data, 0, byteString.Length);
        }

        /// <inheritdoc cref="Stream.WriteAsync(byte[], int, int, CancellationToken)"/>
        /// <param name="stream">The instance of <see cref="Stream"/> to write to.</param>
        /// <param name="byteString">The byte string to write to <paramref name="stream"/>.</param>
        public static Task WriteAsync(this Stream stream, ByteString byteString, CancellationToken cancellationToken)
        {
            return stream.WriteAsync(byteString.Data, 0, byteString.Length, cancellationToken);
        }

        /// <inheritdoc cref="Stream.Write(byte[], int, int)"/>
        /// <param name="stream">The instance of <see cref="Stream"/> to write to.</param>
        /// <param name="byteString">The byte string to write to <paramref name="stream"/>.</param>
        public static void Write(this Stream stream, ByteString byteString, int offset, int count)
        {
            stream.Write(byteString.Data, offset, count);
        }

        /// <inheritdoc cref="Stream.WriteAsync(byte[], int, int)"/>
        /// <param name="stream">The instance of <see cref="Stream"/> to write to.</param>
        /// <param name="byteString">The byte string to write to <paramref name="stream"/>.</param>
        public static Task WriteAsync(this Stream stream, ByteString byteString, int offset, int count)
        {
            return stream.WriteAsync(byteString.Data, offset, count);
        }

        /// <inheritdoc cref="Stream.WriteAsync(byte[], int, int, CancellationToken)"/>
        /// <param name="stream">The instance of <see cref="Stream"/> to write to.</param>
        /// <param name="byteString">The byte string to write to <paramref name="stream"/>.</param>
        public static Task WriteAsync(this Stream stream, ByteString byteString, int offset, int count, CancellationToken cancellationToken)
        {
            return stream.WriteAsync(byteString.Data, offset, count, cancellationToken);
        }

        /// <inheritdoc cref="Encoding.GetString(byte[])"/>
        /// <param name="encoding">The instance of <see cref="Encoding"/> to decode with.</param>
        /// <param name="byteString">The byte string to decode.</param>
        public static string GetString(this Encoding encoding, ByteString byteString)
        {
            return encoding.GetString(byteString.Data);
        }

        /// <inheritdoc cref="Encoding.GetString(byte[], int, int)"/>
        /// <param name="encoding">The instance of <see cref="Encoding"/> to decode with.</param>
        /// <param name="byteString">The byte string to decode.</param>
        public static string GetString(this Encoding encoding, ByteString byteString, int index, int count)
        {
            return encoding.GetString(byteString.Data, index, count);
        }

        /// <inheritdoc cref="Decoder.GetCharCount(byte[], int, int, bool)"/>
        /// <param name="decoder">The instance of <see cref="Decoder"/> to decode with.</param>
        /// <param name="byteString">The byte string to decode.</param>
        public static int GetCharCount(this Decoder decoder, ByteString byteString, int index, int count, bool flush)
        {
            return decoder.GetCharCount(byteString.Data, index, count, flush);
        }

        /// <inheritdoc cref="Decoder.GetChars(byte[], int, int, char[], int, bool)"/>
        /// <param name="decoder">The instance of <see cref="Decoder"/> to decode with.</param>
        /// <param name="byteString">The byte string to decode.</param>
        public static int GetChars(this Decoder decoder, ByteString byteString, int byteIndex, int byteCount, char[] chars, int charIndex, bool flush)
        {
            return decoder.GetChars(byteString.Data, byteIndex, byteCount, chars, charIndex, flush);
        }
    }
}
