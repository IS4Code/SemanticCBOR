// Modified https://raw.githubusercontent.com/terrafx/terrafx/main/sources/Core/Collections/ValueStack%601.cs
// Copyright © Tanner Gooding and Contributors. Licensed under the MIT License (MIT). See License.md in the repository root for more information.

// This file includes code based on the Stack<T> class from https://github.com/dotnet/runtime/
// The original code is Copyright © .NET Foundation and Contributors. All rights reserved. Licensed under the MIT License (MIT).

using System;
using System.Diagnostics;
using System.Diagnostics.CodeAnalysis;
using System.Runtime.CompilerServices;

/// <summary>Represents a stack of items.</summary>
/// <typeparam name="T">The type of the items contained in the stack.</typeparam>
/// <remarks>This type is meant to be used as an implementation detail of another type and should not be part of your public surface area.</remarks>
[DebuggerDisplay("Capacity = {Capacity}; Count = {Count}")]
partial struct ValueStack<T> : IEquatable<ValueStack<T>> where T : unmanaged
{
    internal T[] _items;
    internal int _count;

    /// <summary>Whether this instance is initialized.</summary>
    public readonly bool IsInitialized => _items != null;

    /// <summary>Initializes a new instance of the <see cref="ValueStack{T}" /> struct.</summary>
    public ValueStack()
    {
        _items = Array.Empty<T>();
    }

    /// <summary>Gets the number of items that can be contained by the stack without being resized.</summary>
    public readonly int Capacity {
        get {
            var items = _items;
            return items is not null ? items.Length : 0;
        }
    }

    /// <summary>Gets the number of items contained in the stack.</summary>
    public readonly int Count => _count;

    /// <summary>Compares two <see cref="ValueStack{T}" /> instances to determine equality.</summary>
    /// <param name="left">The <see cref="ValueStack{T}" /> to compare with <paramref name="right" />.</param>
    /// <param name="right">The <see cref="ValueStack{T}" /> to compare with <paramref name="left" />.</param>
    /// <returns><c>true</c> if <paramref name="left" /> and <paramref name="right" /> are equal; otherwise, false.</returns>
    public static bool operator ==(ValueStack<T> left, ValueStack<T> right)
    {
        return (left._items == right._items)
            && (left._count == right._count);
    }

    /// <summary>Compares two <see cref="ValueStack{T}" /> instances to determine inequality.</summary>
    /// <param name="left">The <see cref="ValueStack{T}" /> to compare with <paramref name="right" />.</param>
    /// <param name="right">The <see cref="ValueStack{T}" /> to compare with <paramref name="left" />.</param>
    /// <returns><c>true</c> if <paramref name="left" /> and <paramref name="right" /> are not equal; otherwise, false.</returns>
    public static bool operator !=(ValueStack<T> left, ValueStack<T> right) => !(left == right);

    /// <inheritdoc />
    public override readonly bool Equals([NotNullWhen(true)] object? obj) => (obj is ValueStack<T> other) && Equals(other);

    /// <inheritdoc />
    public readonly bool Equals(ValueStack<T> other) => this == other;

    /// <inheritdoc />
    public override readonly int GetHashCode() => HashCode.Combine(_items, _count);
}

/// <summary>Provides functionality for the <see cref="ValueStack{T}" /> struct.</summary>
static class ValueStack
{
    /// <summary>Gets an empty stack.</summary>
    public static ValueStack<T> Empty<T>() where T : unmanaged => new ValueStack<T>();

    /// <summary>Removes all items from the stack.</summary>
    /// <param name="stack">The stack which should be cleared.</param>
    public static void Clear<T>(this ref ValueStack<T> stack) where T : unmanaged
    {
        stack._count = 0;
    }

    /// <summary>Ensures the capacity of the stack is at least the specified value.</summary>
    /// <param name="stack">The stack whose capacity should be ensured.</param>
    /// <param name="capacity">The minimum capacity the stack should support.</param>
    /// <remarks>This method does not throw if <paramref name="capacity" /> is negative and instead does nothing.</remarks>
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static void EnsureCapacity<T>(this ref ValueStack<T> stack, int capacity) where T : unmanaged
    {
        var currentCapacity = stack.Capacity;

        if(capacity > currentCapacity)
        {
            stack.Resize(capacity, currentCapacity);
        }
    }

    /// <summary>Pushes an item to the top of the stack.</summary>
    /// <param name="stack">The stack to which the item should be pushed.</param>
    /// <param name="item">The item to push to the top of the stack.</param>
    public static void Push<T>(this ref ValueStack<T> stack, T item) where T : unmanaged
    {
        var count = stack._count;
        var newCount = count + 1;

        stack.EnsureCapacity(count + 1);

        stack._count = newCount;
        stack._items[count] = item;
    }

    /// <summary>Tries to pop an item from the top of the stack.</summary>
    /// <param name="stack">The stack from which the item should be popped.</param>
    /// <param name="item">When <c>true</c> is returned, this contains the item from the top of the stack.</param>
    /// <returns><c>true</c> if the stack was not empty; otherwise, <c>false</c>.</returns>
    public static bool TryPop<T>(this ref ValueStack<T> stack, [MaybeNullWhen(false)] out T item) where T : unmanaged
    {
        var count = stack._count;
        var newCount = count - 1;

        if(count == 0)
        {
            item = default!;
            return false;
        }

        stack._count = newCount;
        item = stack._items[newCount];

        return true;
    }

    internal static void Resize<T>(this ref ValueStack<T> stack, int capacity, int currentCapacity) where T : unmanaged
    {
        var newCapacity = Math.Max(capacity, currentCapacity * 2);
        Array.Resize(ref stack._items, newCapacity);
    }
}
